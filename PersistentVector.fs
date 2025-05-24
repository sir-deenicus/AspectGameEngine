// vector implementation ported from https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Vector.java
// based on https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/PersistentVector.fs
// adjustments pair programmed with gemini
namespace FSharpx.Collections
open System.Threading 

module Literals =

    [<Literal>]
    let internal blockSizeShift = 5 // TODO: what can we do in 64Bit case?

    [<Literal>]
    let internal blockSize = 32

    [<Literal>]
    let internal blockIndexMask = 0x01f

type Node =
    // Immutable fields once the Node is constructed.
    val Array : obj[]
    // The 'EditSessionId' ref cell is the core of the ownership mechanism.
    // - If '!(EditSessionId)' is null, the node is considered persistent/frozen.
    // - If '!(EditSessionId)' is non-null, that non-null object is the ID of an active transient session.
    // - Nodes belonging to the *same* transient editing session will have their 'EditSessionId'
    //   field be the *exact same ref cell* as the 'ownerSessionIdRef' of their owning TransientVector.
    val EditSessionId : ref<obj>

    // Constructor 1: For creating a "persistent" node (or one not yet owned by a transient).
    // This node gets its own unique 'EditSessionId' ref cell, and that cell is initialized to hold 'null'.
    new() =
        { Array = Array.create Literals.blockSize null; EditSessionId = ref null }

    // Constructor 2: For creating a new node that is immediately "owned" by a specific transient session.
    // 'transientSessionIdRef' IS the 'ownerSessionIdRef' (the identity ref cell) from the TransientVector
    // that is creating/adopting this node. The node's 'EditSessionId' field becomes this shared ref cell.
    // 'nodeArrayData' is the obj[] to be used for this node's Array.
    internal new(transientSessionIdRef: ref<obj>, nodeArrayData: obj[]) =
        { Array = nodeArrayData; EditSessionId = transientSessionIdRef }

    // Static Factory Method: For creating the initial root Node AND its unique session ID ref
    // when a brand new, empty TransientVector is being constructed.
    // Returns: A tuple containing (theNewNode, theSessionIdRefCellForThatNodeAndTransient).
    static member NewRootForTransientSession() =
        let newSessionMarkerObject = obj() // The actual object used as marker doesn't matter, its reference is unique.
        let sessionRefCell = ref newSessionMarkerObject // This ref cell IS the session ID.
        
        // Create the node using Constructor 2, passing the session's ref cell and a new array.
        // This means the new node's 'EditSessionId' field *is* 'sessionRefCell'.
        let node = Node(sessionRefCell, Array.create Literals.blockSize null)
        
        node, sessionRefCell // Return the node and the session ID ref cell 

//-------------------------------------------------------------------------
// TransientVector<'T> (Non-Fable) - FULL IMPLEMENTATION
//-------------------------------------------------------------------------
type internal TransientVector<'T> =
    // Mutable fields that define the state of the transient vector
    val mutable Count : int
    val mutable Shift : int
    val mutable Root  : Node // This root might initially be a persistent node
    val mutable Tail  : obj[]

    // ownerSessionIdRef is the unique identity (a ref cell) for this transient editing session.
    // If !(ownerSessionIdRef) is null, the transient has been finalized and should not be used.
    val OwnerSessionIdRef : ref<obj>

    // Constructor 1: For creating a brand new, empty TransientVector.
    // It gets a new root node and a new session ID.
    new() =
        let initialRootNode, newOwnerSessionIdRef = Node.NewRootForTransientSession()
        { Count = 0;
          Shift = Literals.blockSizeShift;
          Root = initialRootNode;
          Tail = Array.create Literals.blockSize null;
          OwnerSessionIdRef = newOwnerSessionIdRef }

    // Constructor 2: For creating a TransientVector from an existing PersistentVector's state.
    // Used by PersistentVector.AsTransient().
    // 'initialRoot' comes from the PersistentVector and is likely persistent (its EditSessionId contains null).
    // 'newOwnerSessionIdRef' is a fresh session ID ref created by AsTransient().
    // internal new(initialCount: int, initialShift: int, initialRootFromPV: Node, initialTailFromPV: obj[], newOwnerSessionIdRef: ref<obj>) =
    //     { Count = initialCount;
    //       Shift = initialShift;
    //       Root = initialRootFromPV; // Will be copied on first write by EnsureSessionValidAndRootOwned
    //       Tail = initialTailFromPV; // Tail is always copied for the transient
    //       OwnerSessionIdRef = newOwnerSessionIdRef }

    internal new(initialCount: int, initialShift: int, initialRootFromPV: Node, initialTailFromPV: obj[], newOwnerSessionIdRef: ref<obj>) =
        // VITAL FIX: Always create a full-size tail buffer for the transient.
        let transientTailArray = Array.create Literals.blockSize null
        // Copy the contents of the (potentially shorter) tail from PersistentVector into the new buffer.
        Array.blit initialTailFromPV 0 transientTailArray 0 initialTailFromPV.Length

        { Count = initialCount;
        Shift = initialShift;
        Root = initialRootFromPV; // Will be copied on first write by EnsureSessionValidAndRootOwned
        Tail = transientTailArray; // Use the full-size, correctly populated buffer
        OwnerSessionIdRef = newOwnerSessionIdRef }

    // --- Internal Helper Methods for Ownership and Tree Manipulation ---

    // Ensures the transient session is still active (its ID ref isn't null).
    // Also ensures that 'this.Root' field points to a Node that is definitively "owned"
    // by this transient session (i.e., root.EditSessionId IS this.OwnerSessionIdRef).
    // If root was from a persistent structure, it's copied here, and the copy becomes the new root.
    member internal this.EnsureSessionValidAndRootOwned() =
        if !(this.OwnerSessionIdRef) = null then
            failwith "TransientVector operation on a finalized (persisted) transient."

        // If this.Root.EditSessionId is not the *same reference cell* as this.OwnerSessionIdRef,
        // it means 'this.Root' is either a persistent node or belongs to a different transient.
        // It must be copied to become owned by *this* transient session.
        if not (System.Object.ReferenceEquals(this.Root.EditSessionId, this.OwnerSessionIdRef)) then
             this.Root <- Node(this.OwnerSessionIdRef, Array.copy this.Root.Array)


    // Ensures a given 'nodeToEnsure' is "owned" by this transient session.
    // If not, it's copied, and the copy is returned. The copy's EditSessionId will be this.OwnerSessionIdRef.
    // If already owned, the original nodeToEnsure is returned.
    member internal this.EnsureNodeOwned(nodeToEnsure: Node) : Node =
        // First, ensure the overall transient session is still valid.
        if !(this.OwnerSessionIdRef) = null then
            failwith "TransientVector operation on a finalized transient (while ensuring node ownership)."

        if System.Object.ReferenceEquals(nodeToEnsure.EditSessionId, this.OwnerSessionIdRef) then
            nodeToEnsure // Already owned by this specific transient session.
        else
            // Not owned by this session. Copy it.
            // The new Node's EditSessionId field becomes this transient's OwnerSessionIdRef.
            Node(this.OwnerSessionIdRef, Array.copy nodeToEnsure.Array)

    // Creates a new path of nodes (owned by this transient) leading to the 'leafNode'.
    // 'leafNode' itself will also be ensured to be owned by this transient.
    member internal this.NewPathOwned(level: int, leafNode: Node) : Node =
        if level = 0 then
            this.EnsureNodeOwned leafNode // Ensure the leaf node is owned by this session
        else
            let newPathSegmentArray = Array.create Literals.blockSize null
            // Recursively create the rest of the path; the result will be an owned node.
            newPathSegmentArray.[0] <- this.NewPathOwned(level - Literals.blockSizeShift, leafNode) :> obj
            // This new path segment node is created as owned by this transient session.
            Node(this.OwnerSessionIdRef, newPathSegmentArray)

    // Pushes 'nodeToPushToTree' (which represents a full block, like a former tail)
    // into the tree structure rooted at 'parentNode'.
    // All modified or newly created nodes in the path become owned by this transient.
    member internal this.PushTailOwned(level: int, parentNode: Node, nodeToPushToTree: Node) : Node =
        let editableParent = this.EnsureNodeOwned parentNode // Parent in the path is made owned
        let subidx = ((this.Count - 1) >>> level) &&& Literals.blockIndexMask

        let nodeToInsertInParent =
            if level = Literals.blockSizeShift then
                // nodeToPushToTree is at the correct level to be inserted directly. Ensure it's owned.
                this.EnsureNodeOwned nodeToPushToTree
            else
                // Need to go deeper.
                let childNodeOrNull = editableParent.Array.[subidx]
                if childNodeOrNull <> null then
                    // Child exists, recurse down this path.
                    this.PushTailOwned(level - Literals.blockSizeShift, childNodeOrNull :?> Node, nodeToPushToTree)
                else
                    // No child at this spot, create a new path for nodeToPushToTree.
                    this.NewPathOwned(level - Literals.blockSizeShift, nodeToPushToTree)

        editableParent.Array.[subidx] <- nodeToInsertInParent :> obj
        editableParent // Return the (potentially new) editable parent

    // Performs an in-place update (association) within the transient's tree.
    // Assumes the path to the element exists.
    // All nodes in the path are ensured to be owned by this transient.
    member internal this.DoAssocInPlace(currentLevel: int, nodeToUpdate: Node, indexInVector: int, valueToSet: obj) : Node =
        let editableNode = this.EnsureNodeOwned nodeToUpdate // Ensure current node in path is owned

        if currentLevel = 0 then // Leaf level of the tree (node contains direct items)
            editableNode.Array.[indexInVector &&& Literals.blockIndexMask] <- valueToSet
        else // Internal node in the tree
            let subidx = (indexInVector >>> currentLevel) &&& Literals.blockIndexMask
            let childNodeToRecurse = editableNode.Array.[subidx] :?> Node
            // Recursively call, result is an owned child node.
            let updatedChildNode = this.DoAssocInPlace(currentLevel - Literals.blockSizeShift, childNodeToRecurse, indexInVector, valueToSet)
            editableNode.Array.[subidx] <- updatedChildNode :> obj // Place owned child back
        editableNode // Return the (potentially new) owned node

    // --- Publicly Accessible Methods ---
    member internal this.TailOff() : int =
        if this.Count < Literals.blockSize then 0
        else ((this.Count - 1) >>> Literals.blockSizeShift) <<< Literals.blockSizeShift

    // Adds an item to the end of the transient vector (mutates in place).
    member this.Conj(item: 'T) : TransientVector<'T> =
        this.EnsureSessionValidAndRootOwned() // Ensure transient is valid and its root is prepared for modification.

        let currentTailSize = this.Count - this.TailOff()
        if currentTailSize < Literals.blockSize then // Space in the current tail array
            this.Tail.[this.Count &&& Literals.blockIndexMask] <- item :> obj
        else // Tail is full, need to push it into the tree.
            // The current 'Tail' array becomes a new node in the tree.
            // This new node must be "owned" by this transient session.
            let oldTailAsNewTreeNode = Node(this.OwnerSessionIdRef, this.Tail)

            // Allocate a new tail array for the transient.
            let newTailArrayForTransient = Array.create Literals.blockSize null
            newTailArrayForTransient.[0] <- item :> obj
            this.Tail <- newTailArrayForTransient // Update transient's mutable Tail field

            let newRootNodeAfterPush =
                // Check if the root itself needs to expand (tree grows a level taller).
                if (this.Count >>> Literals.blockSizeShift) > (1 <<< this.Shift) then
                    // Create a new overall root for the transient, owned by this session.
                    let newOverallRoot = Node(this.OwnerSessionIdRef, Array.create Literals.blockSize null)
                    newOverallRoot.Array.[0] <- this.Root :> obj // Current (now owned) root becomes child[0]
                    // Create a new path for the oldTailAsNewTreeNode, also owned.
                    newOverallRoot.Array.[1] <- this.NewPathOwned(this.Shift, oldTailAsNewTreeNode) :> obj
                    this.Shift <- this.Shift + Literals.blockSizeShift // Update mutable Shift
                    newOverallRoot
                else
                    // Root has space, push the oldTailAsNewTreeNode into the existing tree structure.
                    this.PushTailOwned(this.Shift, this.Root, oldTailAsNewTreeNode)
            this.Root <- newRootNodeAfterPush // Update transient's mutable Root field

        this.Count <- this.Count + 1
        this // Return self for potential chaining (though typically mutation is void)

    // Updates an item at a specific index (mutates in place).  
    member this.UpdateInPlace(index: int, value: 'TItem) : TransientVector<'T> =
        this.EnsureSessionValidAndRootOwned()

        if index >= 0 && index < this.Count then // Valid index for update *only*
            if index >= this.TailOff() then // Update is in the Tail array
                this.Tail.[index &&& Literals.blockIndexMask] <- value :> obj
            else // Update is in the tree structure
                this.Root <- this.DoAssocInPlace(this.Shift, this.Root, index, value :> obj)
        else
            // Strictly an update, so out-of-bounds is an error.
            raise(System.IndexOutOfRangeException(sprintf "UpdateInPlace: Index %d is out of bounds for transient vector of count %d." index this.Count))
        this

    // Finalizes the transient vector into an immutable PersistentVector.
    // This transient instance should not be used after calling persistent().
    member this.Persistent() : PersistentVector<'T> =
        if !(this.OwnerSessionIdRef) = null then // Check if already finalized
            failwith "persistent! called twice on transient or on an invalid/already finalized one."

        // This ensures the root node is consistent with the session, important if no mutations occurred
        // that would have already triggered EnsureSessionValidAndRootOwned.
        this.EnsureSessionValidAndRootOwned()

        // ** THE KEY FREEZING STEP **
        // Invalidate this transient session by setting its shared session ID ref cell to null.
        // Any Node whose .EditSessionId field *was* this 'OwnerSessionIdRef' reference cell
        // will now have '!(node.EditSessionId)' evaluate to 'null'.
        // This effectively "freezes" all nodes that belonged to this transient session.
        this.OwnerSessionIdRef := null

        // The 'this.Root' node (which is now ensured to be "owned" by this session, meaning
        // its 'root.EditSessionId' IS 'this.OwnerSessionIdRef') is now considered frozen
        // because '!(this.Root.EditSessionId)' is now null.

        let finalTailLength = this.Count - this.TailOff()
        // Create a new, exactly sized tail array for the PersistentVector.
        let trimmedTailForPersistent = Array.zeroCreate finalTailLength // More efficient if elements are value types or known null
        Array.blit this.Tail 0 trimmedTailForPersistent 0 finalTailLength

        // Pass the (now frozen) root, and other state to PersistentVector constructor.
        // This assumes PersistentVector has a constructor like:
        // new(count, shift, root, tail)
        PersistentVector<'T>(this.Count, this.Shift, this.Root, trimmedTailForPersistent)


    // --- IEnumerable / Ranged Iterator ---
    // (Needed for operations like PersistentVector.ofSeq which uses TransientVector.Conj)
    // Helper to get the array containing the element at index 'i' (for reading).
    member internal this.ArrayFor(i: int) : obj[] =
        if i >= 0 && i < this.Count then
            if i >= this.TailOff() then
                this.Tail
            else
                let mutable currentNode = this.Root
                let mutable currentLevel = this.Shift
                while currentLevel > 0 do
                    let pos = (i >>> currentLevel) &&& Literals.blockIndexMask
                    currentNode <- currentNode.Array.[pos] :?> Node
                    currentLevel <- currentLevel - Literals.blockSizeShift
                currentNode.Array
        else
            // This case should ideally be guarded by callers, but if not:
            raise(System.IndexOutOfRangeException("ArrayFor: Index out of bounds during iteration or access."))

    member private this.RangedIterator(startIndex: int, endIndex: int) : 'T seq =
        // Ensure endIndex is capped at Count to prevent out-of-bounds if iterator is misused.
        let actualEndIndex = min endIndex this.Count
        let mutable i = startIndex
        // Initialize currentBlockStartIndex correctly even if startIndex is 0.
        let mutable currentBlockStartIndex = if i < this.Count then i - (i % Literals.blockSize) else 0
        let mutable arrayForBlock = if startIndex < actualEndIndex then (this.ArrayFor i) else null

        seq {
            while i < actualEndIndex do // Iterate up to actualEndIndex
                if arrayForBlock <> null && (i - currentBlockStartIndex = Literals.blockSize) then
                    // Moved to a new block (and not the first iteration if i=0)
                    arrayForBlock <- this.ArrayFor i
                    currentBlockStartIndex <- i // i is the start of the new block
                
                if arrayForBlock = null && i < actualEndIndex then // Should only happen if initial arrayForBlock was null
                    arrayForBlock <- this.ArrayFor i
                    currentBlockStartIndex <- i - (i % Literals.blockSize)


                if arrayForBlock <> null then // Additional safety, arrayForBlock should be non-null here
                    yield arrayForBlock.[i &&& Literals.blockIndexMask] :?> 'T
                // else: if arrayForBlock is still null, it means i >= count, loop should terminate.

                i <- i + 1
        }

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() = (this.RangedIterator(0, this.Count)).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = (this.RangedIterator(0, this.Count) :> System.Collections.IEnumerable).GetEnumerator()
 
and PersistentVector<'T>(persistentCount: int, persistentShift: int, persistentRoot: Node, persistentTail: obj[]) =
    // Ensure that the root node received is indeed persistent (or considered frozen).
    // This is a runtime check; ideally, types would enforce this more strongly if possible.
    // For nodes created by Node(), !(node.EditSessionId) will be null.
    // For nodes coming from a finalized TransientVector, !(node.EditSessionId) will also be null.
    do
        if !(persistentRoot.EditSessionId) <> null then
             failwith "PersistentVector cannot be constructed with a root node that appears to belong to an active transient session."

    // Fields are implicitly created from constructor parameters if not shadowed by 'let' bindings.
    // To make them accessible as properties or for internal use, we can re-declare or use them directly.
    let count: int = persistentCount
    let shift: int = persistentShift
    let root: Node = persistentRoot
    let tail: obj[] = persistentTail // This tail is a copy, specific to this PV instance.

    // Lazily computed hash code
    let mutable hashCodeOpt: int option = None

    // Pre-calculated tailOffset for this persistent vector instance
    let tailOff: int =
        if count < Literals.blockSize then 0
        else ((count - 1) >>> Literals.blockSizeShift) <<< Literals.blockSizeShift

    static let emptyInstance = PersistentVector<'T>(0, Literals.blockSizeShift, Node(), [||])
    
    // --- Static Members --- 
    static member Empty() : PersistentVector<'T> = emptyInstance

    static member internal DetermineShiftForCount(newCount: int) : int =
        if newCount <= Literals.blockSize then
            Literals.blockSizeShift
        else
            let mutable idx = (newCount - 1) >>> Literals.blockSizeShift
            let mutable newShiftVal = Literals.blockSizeShift
            while (idx >>> newShiftVal) > 0 do
                newShiftVal <- newShiftVal + Literals.blockSizeShift
            newShiftVal

    member this.Length : int = count

    member this.IsEmpty : bool = (count = 0)

    // Internal helper to get the array containing the element at index 'i'.
    // This is primarily for read access within PersistentVector operations.
    member internal this.ArrayFor(i: int) : obj[] =
        if i >= 0 && i < count then
            if i >= tailOff then // Element is in the tail array
                tail
            else // Element is in the tree structure
                let mutable currentNode = root
                let mutable currentLevel = shift
                while currentLevel > 0 do
                    let pos = (i >>> currentLevel) &&& Literals.blockIndexMask
                    let childNodeOrNull = currentNode.Array.[pos]
                    if childNodeOrNull = null then
                        // This indicates a corrupt or unexpectedly sparse tree for a valid index 'i'.
                        // Should not happen in a correctly constructed vector if i < count.
                        raise(System.InvalidOperationException(sprintf "PersistentVector internal error: Null child node encountered in tree path for valid index %d." i))
                    currentNode <- childNodeOrNull :?> Node
                    currentLevel <- currentLevel - Literals.blockSizeShift
                currentNode.Array
        else
            raise(System.IndexOutOfRangeException(sprintf "Index %d is out of bounds for PersistentVector of length %d." i count))

    member this.Item
        with get (i: int) : 'T =
            let nodeArray = this.ArrayFor i
            nodeArray.[i &&& Literals.blockIndexMask] :?> 'T

    // Provides a sequence view of a range of elements in the vector.
    // This is the primary IEnumerable implementation detail.
    member this.RangedIterator(startIndex: int, endIndex: int) : 'T seq =
        // Ensure startIndex and endIndex are within valid bounds for iteration.
        let actualStartIndex = max 0 startIndex
        let actualEndIndex = min count endIndex // Cap endIndex at vector's count

        let mutable i = actualStartIndex
        // Initialize currentBlockStartIndex correctly, considering if actualStartIndex is 0 or beyond.
        let mutable currentBlockStartIndex =
            if i < actualEndIndex then i - (i % Literals.blockSize)
            else 0 // Default if range is empty or invalid

        // Initialize arrayForBlock. It might be null if the range is empty or starts beyond count.
        let mutable arrayForBlock =
            if i < actualEndIndex then (this.ArrayFor i)
            else null

        seq {
            while i < actualEndIndex do // Iterate up to the capped actualEndIndex
                // Check if we've crossed a block boundary.
                // 'arrayForBlock' should not be null here if i < actualEndIndex.
                if arrayForBlock <> null && (i - currentBlockStartIndex = Literals.blockSize) then
                    arrayForBlock <- this.ArrayFor i
                    currentBlockStartIndex <- i // 'i' is now the start of this new block

                // If, for some reason (e.g., initial empty range), arrayForBlock is null, fetch it.
                // This also covers the first iteration if currentBlockStartIndex was not perfectly aligned.
                if arrayForBlock = null && i < actualEndIndex then
                    arrayForBlock <- this.ArrayFor i
                    currentBlockStartIndex <- i - (i % Literals.blockSize)


                if arrayForBlock <> null then
                    yield arrayForBlock.[i &&& Literals.blockIndexMask] :?> 'T
                // else: If arrayForBlock is still null, it means i >= count (or actualEndIndex was 0),
                // and the loop condition 'i < actualEndIndex' should handle termination.

                i <- i + 1
        }

    // --- GetHashCode and Equals (can be included in this first part) ---
    override this.GetHashCode() =
        match hashCodeOpt with
        | Some hash -> hash
        | None ->
            let mutable hash = 1
            // Use the RangedIterator to iterate for hashing
            for x in this.RangedIterator(0, count) do
                hash <- 31 * hash + Unchecked.hash x // Assuming 'Unchecked.hash' is appropriate
            hashCodeOpt <- Some hash
            hash

    override this.Equals(otherObj: obj) =
        match otherObj with
        | :? PersistentVector<'T> as otherVector ->
            if System.Object.ReferenceEquals(this, otherVector) then true
            elif this.Length <> otherVector.Length then false
            // Optimization: if hash codes are computed and differ, they can't be equal.
            // (Requires GetHashCode to be called on both if not already computed).
            elif this.GetHashCode() <> otherVector.GetHashCode() then false
            else
                // Element-wise comparison using the iterators
                Seq.forall2 (Unchecked.equals) (this.RangedIterator(0, count)) (otherVector.RangedIterator(0, otherVector.Length))
        | _ -> false

    // --- Interface Implementations (can be included) ---
    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = count

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() = (this.RangedIterator(0, count)).GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() = (this.RangedIterator(0, count) :> System.Collections.IEnumerable).GetEnumerator()

    // Internal method for setting hash, used by FSharpx.Collections Map/Set hashing
    member internal this.SetHash(hash) =
        hashCodeOpt <- hash
        this

    // More internal helpers for tree manipulation (will be used by Conj, Update, Initial etc.)
    // These helpers create NEW PERSISTENT nodes.
    member internal this.CreateNewPersistentPath(level: int, leafNodeToAdopt: Node) : Node =
        // 'leafNodeToAdopt' is assumed to be a persistent node (e.g., a former tail converted to a node).
        if level = 0 then
            leafNodeToAdopt
        else
            let newPersistentPathNode = Node() // Creates a node with !(EditSessionId) = null
            newPersistentPathNode.Array.[0] <- this.CreateNewPersistentPath(level - Literals.blockSizeShift, leafNodeToAdopt) :> obj
            newPersistentPathNode

    member internal this.PushTailToPersistentTree(level: int, currentParentNode: Node, fullTailAsPersistentNode: Node) : Node =
        // 'currentParentNode' is from the existing persistent structure.
        // 'fullTailAsPersistentNode' is a new persistent node representing the data to insert.

        let subIndexInParent = ((this.Length - 1) >>> level) &&& Literals.blockIndexMask

        // Create a new persistent node for the modified path. Its EditSessionId will be a new ref cell containing null.
        let newParentNodeCopy = Node()
        // Copy the array content from the original parent.
        Array.blit currentParentNode.Array 0 newParentNodeCopy.Array 0 currentParentNode.Array.Length

        let actualNodeToPlaceInCopy =
            if level = Literals.blockSizeShift then
                // At the lowest tree level (above data blocks), we insert the fullTailAsPersistentNode directly.
                fullTailAsPersistentNode
            else
                // We are higher in the tree.
                let childNodeFromOriginalParent = currentParentNode.Array.[subIndexInParent]
                if childNodeFromOriginalParent <> null then
                    // Recursively push down the existing branch.
                    this.PushTailToPersistentTree(level - Literals.blockSizeShift, childNodeFromOriginalParent :?> Node, fullTailAsPersistentNode)
                else
                    // No existing child at this position, create a new path for fullTailAsPersistentNode.
                    this.CreateNewPersistentPath(level - Literals.blockSizeShift, fullTailAsPersistentNode)

        newParentNodeCopy.Array.[subIndexInParent] <- actualNodeToPlaceInCopy :> obj
        newParentNodeCopy
    
        member this.Conj(x: 'T) : PersistentVector<'T> =
        // Check if there's room in the current tail.
        if count - tailOff < Literals.blockSize then
            // Room in tail. Create a new tail by appending.
            let newTail = Array.append tail [| x :> obj |]
            PersistentVector<'T>(count + 1, shift, root, newTail)
        else
            // Tail is full. Push the current tail into the tree structure.
            // 1. Create a new persistent Node from the current (full) tail array.
            let fullTailAsPersistentNode = Node() // New persistent node: !(EditSessionId) = null
            Array.blit tail 0 fullTailAsPersistentNode.Array 0 tail.Length // Copy contents of old tail

            // 2. Determine the new shift and root for the tree.
            let newShiftAfterAdd, newRootAfterAdd =
                if (count >>> Literals.blockSizeShift) > (1 <<< shift) then // Root overflow? Tree grows taller.
                    let newOverallRoot = Node() // New persistent root node for the taller tree
                    newOverallRoot.Array.[0] <- root :> obj // Old root becomes child[0]
                    // Create a new path for the fullTailAsPersistentNode.
                    newOverallRoot.Array.[1] <- this.CreateNewPersistentPath(shift, fullTailAsPersistentNode) :> obj
                    (shift + Literals.blockSizeShift, newOverallRoot)
                else // Root has space. Push into existing tree height.
                    (shift, this.PushTailToPersistentTree(shift, root, fullTailAsPersistentNode))

            // 3. Create the new PersistentVector. The new tail contains only the new item 'x'.
            PersistentVector<'T>(count + 1, newShiftAfterAdd, newRootAfterAdd, [| x :> obj |])

    // Internal helper for persistent updates (creates a new path).
    member internal this.DoAssoc(currentLevel: int, nodeToCopy: Node, indexInVector: int, valueToSet: obj) : Node =
        // Create a new persistent node for the copied path.
        let newPathNode = Node()
        Array.blit nodeToCopy.Array 0 newPathNode.Array 0 nodeToCopy.Array.Length // Copy content

        if currentLevel = 0 then // Leaf level of the tree
            newPathNode.Array.[indexInVector &&& Literals.blockIndexMask] <- valueToSet
        else // Internal node in the tree
            let subidx = (indexInVector >>> currentLevel) &&& Literals.blockIndexMask
            let childNodeFromOriginal = nodeToCopy.Array.[subidx] :?> Node // Assume child exists
            // Recursively call, result is a new persistent child node.
            newPathNode.Array.[subidx] <- this.DoAssoc(currentLevel - Literals.blockSizeShift, childNodeFromOriginal, indexInVector, valueToSet) :> obj
        newPathNode

    member this.Update(index: int, value: 'T) : PersistentVector<'T> =
        if index >= 0 && index < count then // Valid index for update
            if index >= tailOff then // Update is in the tail array
                let newTail = Array.copy tail
                newTail.[index &&& Literals.blockIndexMask] <- value :> obj
                PersistentVector<'T>(count, shift, root, newTail)
            else // Update is in the tree structure
                let newRoot = this.DoAssoc(shift, root, index, value :> obj)
                PersistentVector<'T>(count, shift, newRoot, tail) // Tail remains the same
        elif index = count then // Index is at the end, effectively an append
            this.Conj(value)
        else
            raise(System.IndexOutOfRangeException(sprintf "Update index %d is out of bounds for PersistentVector of length %d." index count))

    member this.TryUpdate(index: int, value: 'T) : PersistentVector<'T> option =
        if index >= 0 && index <= count then // Allow index = count for append-like behavior
            Some(this.Update(index, value))
        else
            None

    // Internal helper to remove the last element's branch from the tree.
    member internal this.PopTailFromTree(currentLevel: int, nodeToModify: Node) : Node option =
        // 'count - 2' because 'count - 1' is the element being removed.
        // We are finding the path to the *new* last element's block.
        let subidx = ((count - 2) >>> currentLevel) &&& Literals.blockIndexMask // 'count' here needs to be this.Length or the instance's count field

        if currentLevel > Literals.blockSizeShift then // High in the tree
            // It's safer to check if nodeToModify.Array.[subidx] is null before casting,
            // though for a pop operation on a valid vector, it should exist.
            let originalChild = nodeToModify.Array.[subidx] :?> Node 
            let newChildOption = this.PopTailFromTree(currentLevel - Literals.blockSizeShift, originalChild) // Recursive call returns Node option

            match newChildOption with
            | None when subidx = 0 -> // Entire branch below became empty, and this was the 0-th child
                None                   // Signal to prune this node (nodeToModify) as well
            | _ -> // Either child exists (Some), or child is None but subidx > 0 (meaning other children might exist)
                let newParentNode = Node() // New persistent node
                Array.blit nodeToModify.Array 0 newParentNode.Array 0 nodeToModify.Array.Length
                // Store the Option's value (if Some) or null (if None) into the object array.
                // The obj[] can hold 'null', which represents an empty slot.
                newParentNode.Array.[subidx] <- match newChildOption with Some n -> n :> obj | None -> null
                Some newParentNode
        elif subidx = 0 then // We are at a level where children are data blocks (or nodes pointing to them),
                            // and the element being popped was the first in this 'nodeToModify'.
                            // So, 'nodeToModify' itself becomes empty from the perspective of this pop.
            None // Signal to prune this node
        else // Modifying a leaf-level block in the tree (or a node just above data blocks)
            // and subidx > 0, meaning other elements/sub-trees remain in this node.
            let newParentNode = Node() // New persistent node
            Array.blit nodeToModify.Array 0 newParentNode.Array 0 nodeToModify.Array.Length
            // We are not removing 'nodeToModify', just one of its children (which was a data block or points to one).
            // The slot 'subidx' in 'newParentNode' should now be empty.
            newParentNode.Array.[subidx] <- null // Clear the slot for the popped child branch/block.
            Some newParentNode
        // member internal this.PopTailFromTree(currentLevel: int, nodeToModify: Node) : Node option =
        //     // 'count - 2' because 'count - 1' is the element being removed.
        //     // We are finding the path to the *new* last element's block.
        //     let subidx = ((count - 2) >>> currentLevel) &&& Literals.blockIndexMask

        //     if currentLevel > Literals.blockSizeShift then // High in the tree
        //         let originalChild = nodeToModify.Array.[subidx] :?> Node
        //         let newChildOrNull = this.PopTailFromTree(currentLevel - Literals.blockSizeShift, originalChild)

        //         if newChildOrNull.IsNone && subidx = 0 then // Entire branch below became empty
        //             None // Signal to prune this node if it was the only child
        //         else
        //             let newParentNode = Node() // New persistent node
        //             Array.blit nodeToModify.Array 0 newParentNode.Array 0 nodeToModify.Array.Length
        //             newParentNode.Array.[subidx] <- newChildOrNull :> obj // Could be null or a modified child
        //             Some newParentNode
        //     elif subidx = 0 then // Last element in this block was the first, so this block becomes empty
        //         None // Signal to prune this node
        //     else // Modifying a leaf-level block in the tree
        //         let newParentNode = Node() // New persistent node
        //         Array.blit nodeToModify.Array 0 newParentNode.Array 0 nodeToModify.Array.Length
        //         newParentNode.Array.[subidx] <- null // Remove pointer to the (now empty) child that held the last element
        //         Some newParentNode
    
    member this.Initial : PersistentVector<'T> =
        if count = 0 then
            failwith "Cannot get Initial of an empty PersistentVector."
        elif count = 1 then
            PersistentVector<'T>.Empty() // Result is an empty vector
        else
            // At this point, count >= 2.
            // Determine if the element to be removed is in the tail or the tree.
            // 'count - 1' is the index of the last element.
            // 'count - tailOff' is the number of elements in the tail.
            if (count - tailOff) > 1 then
                // Case 1: More than one element in the tail.
                // The last element is in the tail, and removing it leaves a non-empty tail.
                // The root and shift remain unchanged.
                let newTailLength = (count - tailOff) - 1
                let newTail = Array.zeroCreate newTailLength // Or Array.sub tail 0 newTailLength
                Array.blit tail 0 newTail 0 newTailLength
                PersistentVector<'T>(count - 1, shift, root, newTail)
            else
                // Case 2: The last element is the only element in the tail (making tail empty after pop),
                // OR the last element is stored in the tree structure (tail was already empty or just held one item).

                // The new tail for the resulting vector will be the block containing element at index (count - 2).
                // this.ArrayFor() correctly fetches from tree or original tail if (count-2) was there.
                let newTailArray = this.ArrayFor(count - 2)

                // Pop the last element's branch from the tree structure.
                // PopTailFromTree operates based on the state *before* decrementing 'count'.
                // It needs 'this.count' (original count) and 'this.shift' (original shift).
                let newRootOptionAfterPop = this.PopTailFromTree(shift, root)

                // Determine the actual new root and new shift based on PopTailFromTree's result.
                let finalNewRoot, finalNewShift =
                    match newRootOptionAfterPop with
                    | Some rNode -> // The tree is not entirely empty after the pop.
                        // 'rNode' is the root of the modified tree.
                        // Now, check if the tree height (shift) can be reduced.
                        // This happens if the original shift was > minimum, and rNode now only has
                        // a significant child in its 0-th slot, and that child is not "effectively empty".
                        if shift > Literals.blockSizeShift && rNode.Array.[1] = null && rNode.Array.[0] <> null then
                            let candidateNewRoot = rNode.Array.[0] :?> Node

                            // Check if this candidate for the new root is itself an "empty" node shell.
                            // An empty shell would be like a node created by Node() - all array elements are null.
                            let isCandidateEffectivelyEmpty = Array.forall ((=) null) candidateNewRoot.Array

                            if not isCandidateEffectivelyEmpty then
                                // Promote candidateNewRoot and reduce shift.
                                (candidateNewRoot, shift - Literals.blockSizeShift)
                            else
                                // Candidate was empty; don't promote it. Stick with rNode at original shift.
                                (rNode, shift)
                        else
                            // No conditions met for shrinking tree height, or already at minimum shift.
                            (rNode, shift)

                    | None -> // The tree became entirely empty after the pop operation.
                        // The new root is a fresh empty persistent node, and shift is the base shift.
                        (Node(), Literals.blockSizeShift)

                PersistentVector<'T>(count - 1, finalNewShift, finalNewRoot, newTailArray)

    member this.TryInitial : PersistentVector<'T> option =
        if count = 0 then None else Some(this.Initial)

    member this.Last : 'T =
        if count = 0 then failwith "Cannot get Last of an empty PersistentVector."
        else 
            this.[count - 1] // Use the Item indexer

    member this.TryLast : 'T option =
        if count = 0 then None else Some(this.Last)

    member this.Unconj : PersistentVector<'T> * 'T =
        if count = 0 then failwith "Cannot Unconj an empty PersistentVector."
        else (this.Initial, this.Last)

    member this.TryUnconj : (PersistentVector<'T> * 'T) option =
        if count = 0 then None else Some(this.Unconj)

    member this.Rev() : PersistentVector<'T> =
        // Efficient reverse can be done using a transient vector
        if count = 0 then PersistentVector<'T>.Empty()
        else
            let tv = TransientVector<'T>() // Assumes TransientVector is defined and linked with 'and'
            for i = count - 1 downto 0 do
                tv.Conj(this.[i]) |> ignore
            tv.Persistent()

    member this.Take(n: int) : PersistentVector<'T> =
        if n <= 0 then PersistentVector<'T>.Empty ()
        elif n >= count then this
        else
            let originalInstanceTailOff = tailOff // tailOff is pre-calculated for the instance
            if n > originalInstanceTailOff then
                let newTailLength = n - originalInstanceTailOff
                let newTakenTail = tail.[0 .. newTailLength - 1]
                PersistentVector<'T>(n, shift, root, newTakenTail)
            else
                let newShiftForTaken = PersistentVector<'T>.DetermineShiftForCount n
                let newTailOffsetForTakenVector = if n = 0 then 0 else ((n - 1) >>> Literals.blockSizeShift) <<< Literals.blockSizeShift
                let newTailElementsCountForTaken = n - newTailOffsetForTakenVector
                let newTakenTail =
                    if newTailElementsCountForTaken = 0 then [||]
                    else
                        let sourceArrayForNewTail = this.ArrayFor (n - 1)
                        sourceArrayForNewTail.[0 .. newTailElementsCountForTaken - 1]
                let mutable newRootNodeForTaken = root
                let mutable currentOriginalShift = shift
                while currentOriginalShift > newShiftForTaken do
                    newRootNodeForTaken <- newRootNodeForTaken.Array.[0] :?> Node
                    currentOriginalShift <- currentOriginalShift - Literals.blockSizeShift
                PersistentVector<'T>(n, newShiftForTaken, newRootNodeForTaken, newTakenTail)
 
    member internal this.AsTransient() : TransientVector<'T> =
        // This is the crucial link to the TransientVector type.
        // It assumes TransientVector<'T> is defined (likely with 'and').

        // 1. Create a new, unique session ID reference for this new transient session.
        //    The actual object stored in the ref doesn't matter, only its reference identity.
        let newSessionMarkerObject = obj()
        let newOwnerSessionIdRef = ref newSessionMarkerObject

        // 2. Create a new TransientVector instance.
        //    - Pass the current PersistentVector's state (count, shift, root).
        //    - Pass a *copy* of the tail, as the transient will mutate its tail.
        //    - Pass the new unique session ID ref.
        //    This uses the 'internal new(count, shift, root, tail, ownerSessionIdRef)' constructor of TransientVector.
        TransientVector<'T>(count, shift, root, Array.copy tail, newOwnerSessionIdRef)
    
[<RequireQualifiedAccess>]
module PersistentVector =

    // --- Creation ---
    // ... (empty, singleton, ofSeq, init - as previously defined) ...
    let empty<'T> : PersistentVector<'T> = PersistentVector<'T>.Empty()
    let inline singleton (value: 'T) : PersistentVector<'T> = empty.Conj(value)
    let ofSeq (items: 'T seq) : PersistentVector<'T> =
        if Seq.isEmpty items then empty
        else
            let tv = TransientVector<'T>()
            for item in items do tv.Conj(item) |> ignore
            tv.Persistent()
    let init (count: int) (initializer: int -> 'T) : PersistentVector<'T> =
        if count < 0 then invalidArg "count" "Count must be non-negative."
        elif count = 0 then empty
        else
            let tv = TransientVector<'T>()
            for i = 0 to count - 1 do tv.Conj(initializer i) |> ignore
            tv.Persistent()

    // --- Basic Properties and Queries ---
    // ... (isEmpty, length - as previously defined) ...
    let inline isEmpty (vector: PersistentVector<'T>) : bool = vector.IsEmpty
    let inline length (vector: PersistentVector<'T>) : int = vector.Length

    // --- Element Access ---
    // ... (nth, tryNth, head, tryHead, last, tryLast - as previously defined) ...
    let inline nth (index: int) (vector: PersistentVector<'T>) : 'T = vector.[index]
    let tryNth (index: int) (vector: PersistentVector<'T>) : 'T option =
        if index >= 0 && index < vector.Length then Some(vector.[index]) else None
    let head (vector: PersistentVector<'T>) : 'T = if vector.IsEmpty then failwith "Cannot get the head of an empty vector." else vector.[0]
    let tryHead (vector: PersistentVector<'T>) : 'T option = if vector.IsEmpty then None else Some(vector.[0])
    let inline last (vector: PersistentVector<'T>) : 'T = vector.Last
    let inline tryLast (vector: PersistentVector<'T>) : 'T option = vector.TryLast

    // --- Modification (always returns a new vector) ---
    // ... (conj, update, tryUpdate, updateAt, tryUpdateAt, initial, tryInitial, unconj, tryUnconj - as previously defined) ...
    let inline conj (item: 'T) (vector: PersistentVector<'T>) : PersistentVector<'T> = vector.Conj(item)
    let inline update (index: int) (value: 'T) (vector: PersistentVector<'T>) : PersistentVector<'T> = vector.Update(index, value)
    let inline tryUpdate (index: int) (value: 'T) (vector: PersistentVector<'T>) : PersistentVector<'T> option = vector.TryUpdate(index, value)
    let updateAt (index: int) (updater: 'T -> 'T) (vector: PersistentVector<'T>) : PersistentVector<'T> =
        if index >= 0 && index < vector.Length then vector.Update(index, updater vector.[index])
        else raise(System.IndexOutOfRangeException("updateAt: Index out of bounds."))
    let tryUpdateAt (index: int) (updater: 'T -> 'T) (vector: PersistentVector<'T>) : PersistentVector<'T> option =
        if index >= 0 && index < vector.Length then Some(vector.Update(index, updater vector.[index]))
        else None
    let inline initial (vector: PersistentVector<'T>) : PersistentVector<'T> = vector.Initial
    let inline tryInitial (vector: PersistentVector<'T>) : PersistentVector<'T> option = vector.TryInitial
    let inline unconj (vector: PersistentVector<'T>) : PersistentVector<'T> * 'T = vector.Unconj
    let inline tryUnconj (vector: PersistentVector<'T>) : (PersistentVector<'T> * 'T) option = vector.TryUnconj

    /// Appends two persistent vectors, creating a new persistent vector.
    /// This version starts from a transient of the first vector for potential efficiency.
    let append (vectorA: PersistentVector<'T>) (vectorB: PersistentVector<'T>) : PersistentVector<'T> =
        if vectorA.IsEmpty then
            vectorB
        elif vectorB.IsEmpty then
            vectorA
        else
            // Start with a transient version of the first vector.
            let tv = vectorA.AsTransient()

            // Add all elements from vectorB
            for item in vectorB do // vectorB is IEnumerable<'T>
                tv.Conj(item) |> ignore

            tv.Persistent()

    // --- Sub-vectors and Reordering ---
    // ... (take, drop, rev - as previously defined) ...
    let inline take (n: int) (vector: PersistentVector<'T>) : PersistentVector<'T> = vector.Take(n)
    let drop (n: int) (vector: PersistentVector<'T>) : PersistentVector<'T> =
        if n <= 0 then vector
        elif n >= vector.Length then empty
        else
            let tv = TransientVector<'T>()
            for item in vector.RangedIterator(n, vector.Length) do tv.Conj(item) |> ignore
            tv.Persistent()

    let skip (n: int) (vector: PersistentVector<'T>) : PersistentVector<'T> = drop n vector

    let inline rev (vector: PersistentVector<'T>) : PersistentVector<'T> = vector.Rev()

    // --- Iteration and Transformation ---
    // ... (map, mapi, iter, iteri, fold, foldBack, collect, filter, find, tryFind, choose - as previously defined) ...
    let map (mapping: 'T -> 'U) (vector: PersistentVector<'T>) : PersistentVector<'U> =
        if vector.IsEmpty then PersistentVector<'U>.Empty() else
            let tv = TransientVector<'U>()
            for item in vector do tv.Conj(mapping item) |> ignore
            tv.Persistent()
    let mapi (mapping: int -> 'T -> 'U) (vector: PersistentVector<'T>) : PersistentVector<'U> =
        if vector.IsEmpty then PersistentVector<'U>.Empty() else
            let tv = TransientVector<'U>()
            let mutable i = 0
            for item in vector do tv.Conj(mapping i item) |> ignore; i <- i + 1
            tv.Persistent()
    let inline iter (action: 'T -> unit) (vector: PersistentVector<'T>) : unit = Seq.iter action vector
    let inline iteri (action: int -> 'T -> unit) (vector: PersistentVector<'T>) : unit = Seq.iteri action vector
    let fold (folder: 'State -> 'T -> 'State) (initialState: 'State) (vector: PersistentVector<'T>) : 'State =
        Seq.fold folder initialState vector
    let foldBack (folder: 'T -> 'State -> 'State) (vector: PersistentVector<'T>) (initialState: 'State) : 'State =
        vector |> Seq.toList |> List.foldBack folder <| initialState // Simple, but allocates list
    let collect (mapping: 'T -> PersistentVector<'U>) (vector: PersistentVector<'T>) : PersistentVector<'U> =
        if vector.IsEmpty then PersistentVector<'U>.Empty() else
            let tv = TransientVector<'U>()
            for itemT in vector do for itemU in mapping itemT do tv.Conj(itemU) |> ignore
            tv.Persistent()
    let filter (predicate: 'T -> bool) (vector: PersistentVector<'T>) : PersistentVector<'T> =
        if vector.IsEmpty then vector else
            let tv = TransientVector<'T>()
            for item in vector do if predicate item then tv.Conj(item) |> ignore
            if tv.Count = 0 then empty else tv.Persistent()
    let find (predicate: 'T -> bool) (vector: PersistentVector<'T>) : 'T =
        match vector |> Seq.tryFind predicate with Some v -> v | None -> raise (System.Collections.Generic.KeyNotFoundException())
    let tryFind (predicate: 'T -> bool) (vector: PersistentVector<'T>) : 'T option = Seq.tryFind predicate vector
    let choose (chooser: 'T -> 'U option) (vector: PersistentVector<'T>) : PersistentVector<'U> =
        if vector.IsEmpty then PersistentVector<'U>.Empty() else
            let tv = TransientVector<'U>()
            for itemT in vector do match chooser itemT with Some itemU -> tv.Conj(itemU) |> ignore | None -> ()
            if tv.Count = 0 then PersistentVector<'U>.Empty() else tv.Persistent() 

    /// Concatenates a sequence of persistent vectors into a single new persistent vector.
    let concat (vectors: seq<PersistentVector<'T>>) : PersistentVector<'T> =
        // Handle empty input sequence early
        if Seq.isEmpty vectors then
            PersistentVector<'T>.Empty()
        else
            let tv = TransientVector<'T>() // Create one transient for the entire operation
            for vectorInstance in vectors do
                // For each vector in the input sequence, conj its elements to the transient
                for item in vectorInstance do // vectorInstance is IEnumerable<'T>
                    tv.Conj item |> ignore
            
            // If the transient is still empty after processing all vectors (e.g., seq of empty vectors),
            // return the canonical empty persistent vector. Otherwise, persist the transient.
            if tv.Count = 0 then
                PersistentVector<'T>.Empty()
            else
                tv.Persistent()

    // --- Conversion ---
    // ... (toList, toArray, toSeq - as previously defined) ...
    let toList (vector: PersistentVector<'T>) : 'T list = Seq.toList vector
    let toArray (vector: PersistentVector<'T>) : 'T array = Seq.toArray vector
    let inline toSeq (vector: PersistentVector<'T>) : 'T seq = vector :> seq<'T>

    // --- Batch Update ---
    // ... (updateMany - as previously defined) ...
    let updateMany (updates: seq<int * 'TValue>) (vector: PersistentVector<'TValue>) : PersistentVector<'TValue> =
        if Seq.isEmpty updates then vector else
            let transient = vector.AsTransient()
            for (index, valueToSet) in updates do transient.UpdateInPlace(index, valueToSet) |> ignore
            transient.Persistent()

    // --- Active Patterns ---
    // ... (Unconj_Last|Empty| - as previously defined) ...
    let (|Unconj_Last|Empty|) (vector: PersistentVector<'T>) =
        match vector.TryUnconj with Some(i, l) -> Unconj_Last(i, l) | None -> Empty
 
