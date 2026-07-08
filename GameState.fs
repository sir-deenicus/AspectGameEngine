namespace AspectGameEngine

open System.Collections.Generic

module InteractionStrings = 
    [<Literal>]
    let DoorLocked = "door-locked"

    [<Literal>]
    let DoorUnlocked = "door-unlocked" 

[<Struct>]
type InteractResult =
    | Nothing
    | Msg of msgkey:string 

module InteractResult =
    let ofInteractionResult (result: InteractionResult) =
        match result.Message with
        | Some message when result.Succeeded -> InteractResult.Msg message.Key
        | _ -> InteractResult.Nothing

type LookAtResult =
    | Null = 0
    | SingleObject = 1
    | MultipleObjectsFullyListed = 2
    | MultipleObjectsIncompleteList = 3

[<Struct>]
type LookAtInfo = { 
    Result: LookAtResult
    LayerCount: byte
    HasMore: bool
    LayerKey0: string
    LayerKey1: string
    LayerKey2: string
    BaseKey: string 
}

[<Struct>]
type DoorAction =
    | LockDoor of keyid:int
    | UnLockDoor of keyid:int
    | OpenOrCloseDoor 

[<Struct>]
type InteractionType =
    | NullAction
    | DoorInteraction of action: DoorAction
 
type PlayerModel = { 
    mutable PlayerPos: GridPos
    PlayerActorId: int option
    mutable PlayerVisual: PlayerVisualState
    mutable PlayerFrames: NpcFrames
    mutable PlayerState: ActorState
}

type GameModel = {   
    Map: TileMap
    PlayerModel: PlayerModel
    VisibilityState: VisibilityState
    mutable VisibilityHalfWidth: int
    mutable VisibilityHalfHeight: int
    mutable VisibilityTranslucencyBudget: int

    // Per-tile *instance* state keyed by tileIndex (plain int).
    TileComplexStateInstance: Dictionary<int, ComplexState> 
}

type GameMsgs() =
    let _msgs = ResizeArray<string>()

    member _.Add(msg: string) =
        if not (System.String.IsNullOrEmpty(msg)) then
            _msgs.Add(msg)

    member _.GetAll() = 
        _msgs.ToArray()

    member _.GetMostRecent() =
        if _msgs.Count = 0 then None
        else Some _msgs.[_msgs.Count - 1]

    member _.GetRecent(n: int) =
        let count = _msgs.Count
        if count = 0 || n <= 0 then [||]
        else
            let take = min n count
            let result = Array.zeroCreate take
            for i in 0 .. take - 1 do
                result.[i] <- _msgs.[count - take + i]
            result

module Utils =
    let inline inBounds (map: TileMap) (pos: GridPos) =
        pos.X >= 0 && pos.X < map.Width && pos.Y >= 0 && pos.Y < map.Height

    let inline indexOf (map: TileMap) (pos: GridPos) = pos.Y * map.Width + pos.X
    let tryFindVisual (key: string) (visuals: TileVisualEntry[]) : SpriteLoc option =
        visuals
        |> Array.tryFind (fun v -> v.Key = key)
        |> Option.map (fun v -> v.SpriteLoc)

    let inline normalizeKey (key: string) =
        if System.String.IsNullOrEmpty(key) then "" else key

    let tryGetEntityDescKey (id: int) : string =
        match SpritePropsQueries.tryGet id with
        | None -> ""
        | Some sp ->
            match sp.SpriteType with
            | SpriteType.Actor ap -> normalizeKey ap.DescKey
            | SpriteType.Fixture fp -> normalizeKey fp.DescKey
            | SpriteType.Item ip -> normalizeKey ip.DescKey
            | SpriteType.Decal dp -> normalizeKey dp.DescKey

    let tryGetRenderLayer (id: int) : int =
        match SpritePropsQueries.tryGet id with
        | None -> 0
        | Some sp -> sp.RenderLayer
    
    
    /// Search a visuals array for an entry with the given key and return its SpriteLoc.
    /// Returns None if the array is null/empty or no matching key is found.
    let private findVisualSpriteLoc (visuals: TileVisualEntry[]) (key: string) : SpriteLoc option =
        if isNull visuals || visuals.Length = 0 then None
        else
            let mutable i = 0
            let mutable result = Unchecked.defaultof<SpriteLoc>
            let mutable found = false
            while i < visuals.Length && not found do
                if visuals.[i].Key = key then
                    result <- visuals.[i].SpriteLoc
                    found <- true
                i <- i + 1
            if found then Some result else None
   
    
module Doors = 
    
    [<Literal>]
    let private DoorClosedKey = "door-closed"

    [<Literal>]
    let private DoorOpenedKey = "door-opened" 
    
    [<Literal>]
    let private DoorOpenDescKey = "tile.door.open"
    
    let  tryGetTileLocked (model: GameModel) (tileIndex: int) : bool =
        match model.TileComplexStateInstance.TryGetValue(tileIndex) with
        | true, s -> s.IsDoorLocked()
        | _ -> false

    let setTileLocked (model: GameModel) (tileIndex: int) (locked: bool) : unit =
        if locked then
            model.TileComplexStateInstance.[tileIndex] <- ComplexState.ClosedDoor { Locked = true }
        else
            model.TileComplexStateInstance.Remove(tileIndex) |> ignore  

    let inline private tryGetDoorOpenSpriteLoc (props: TileProperties) : SpriteLoc option =
        // Doors always list the "next" (open) visual first, so we can ignore keys entirely.
        if isNull props.Visuals || props.Visuals.Length = 0 then None
        else Some props.Visuals.[0].SpriteLoc

    /// Auto-opens a *closed* door tile at (x,y). Returns true if the tile was changed.
    /// Optimized for being called on every move attempt:
    /// - early-outs for non-doors and already-open doors (by description key)
    /// - only checks lock state when we know we hit a door
    let tryAutoOpenDoorAt (model: GameModel) (x: int) (y: int) : bool =
        let map = model.Map
        if x < 0 || x >= map.Width || y < 0 || y >= map.Height then false
        else
            let props = map.GetTileProperties(x, y)
            if props.TileType <> TileType.Door then false
            elif props.DescriptionKey = DoorOpenDescKey then false // already open (description-driven)
            else
                let tileIndex = y * map.Width + x
                if tryGetTileLocked model tileIndex then false
                else
                    match tryGetDoorOpenSpriteLoc props with
                    | None -> false
                    | Some targetSpriteLoc ->
                        let tile = map.GetTile(x, y)
                        if tile.SpriteLoc = targetSpriteLoc then false
                        else
                            map.Update(x, y, { tile with SpriteLoc = targetSpriteLoc })
                            true

    let tryAutoOpenDoor (model: GameModel) (pos: GridPos) : bool =
        tryAutoOpenDoorAt model pos.X pos.Y

    let private doorMessage key =
        Some { Key = key; Args = [||] }

    let private doorChangedResult pos messageKey visibilityInputChanged occlusionInputChanged =
        { Succeeded = true
          Message = doorMessage messageKey
          TargetPosition = Some pos
          TargetSlot = Some ChangeSlot.BaseTile
          Changes =
            { ChangedBaseCells = [| pos |]
              ChangedLayerCells = [||]
              ChangedEntities =
                [| { Position = pos
                     Slot = ChangeSlot.BaseTile
                     EntityId = None
                     LocalObjectId = None } |]
              VisibilityInputChanged = visibilityInputChanged
              OcclusionInputChanged = occlusionInputChanged
              SaveRelevant = true } }

    let tryInteractDoorResult (model: GameModel) (pos: GridPos) (interaction: DoorAction) : InteractionResult =
        let map = model.Map
        if not (Utils.inBounds map pos) then InteractionResult.Nothing
        else
            let tile = map.GetTile(pos.X, pos.Y)
            let props = map.GetTileProperties(pos.X, pos.Y)
            if props.TileType <> TileType.Door then InteractionResult.Nothing
            else
                let tileSet = TilesetRegistry.get map.TileSetName
                match interaction with  
                | DoorAction.OpenOrCloseDoor ->
                    // Doors always use the first visual entry of the current state as the target state.
                    let tileInfo = tileSet[tile.SpriteLoc]
                    if tileInfo.Visuals.Length = 0 then InteractionResult.Nothing
                    else
                        let visualsInfo = tileInfo.Visuals[0]
                        let oldOpacity = map.GetOpacity(pos.X, pos.Y)
                        map.Update(pos.X, pos.Y, { tile with SpriteLoc = visualsInfo.SpriteLoc })
                        let newOpacity = map.GetOpacity(pos.X, pos.Y)
                        let opacityChanged = oldOpacity <> newOpacity
                        doorChangedResult pos visualsInfo.Key opacityChanged opacityChanged
                | _ -> InteractionResult.Nothing
    
    let tryInteractDoor (model: GameModel) (pos: GridPos) (interaction: DoorAction) : InteractResult =
        tryInteractDoorResult model pos interaction
        |> InteractResult.ofInteractionResult

module GameUpdate = 
    let recomputeVisibility (model: GameModel) : unit =
        RectFov.compute
            model.Map
            model.VisibilityState
            model.PlayerModel.PlayerPos
            model.VisibilityHalfWidth
            model.VisibilityHalfHeight
            model.VisibilityTranslucencyBudget

    let private resolvePlayerSprite (frames: NpcFrames) (visual: PlayerVisualState) : SpriteRef =
        match visual.State, visual.Facing with
        | ActorPose.Attack, ActorFacing.Right -> frames.AttackRight
        | ActorPose.Attack, _ -> frames.AttackLeft
        | _, ActorFacing.Right -> frames.NormalRight
        | _ -> frames.NormalLeft

    let private ensurePlayerActorRegistered (playerActorId: int) (frames: NpcFrames) (visual: PlayerVisualState) : unit =
        let spriteRef = resolvePlayerSprite frames visual
        let actorProps =
            { TileOpacity = TileOpacity.Transparent
              DescKey = "player"
              NpcFrames = Some frames }
        EntityRegistry.SpriteProps.[playerActorId] <-
            { Sprite = spriteRef
              SpriteType = SpriteType.Actor actorProps
              RenderLayer = 100 }

    let private createWith spawnPointIndex (initialMap: TileMap) (playerFrames: NpcFrames) (playerActorId: int option) : GameModel =
        let (x,y) = initialMap.SpawnPoints.[spawnPointIndex] 

        let playerVisual = { Facing = ActorFacing.Left; State = ActorPose.Normal }

        match playerActorId with
        | Some aid ->
            ensurePlayerActorRegistered aid playerFrames playerVisual
            initialMap.SetActor(x, y, aid)
        | None -> ()

        let playerModel = { 
            PlayerPos = GridPos(x,y)
            PlayerActorId = playerActorId
            PlayerVisual = playerVisual
            PlayerFrames = playerFrames
            PlayerState = ActorState.Idle
        }

        let visibilityHalfWidth = max 0 ((initialMap.Width + 1) / 2)
        let visibilityHalfHeight = max 0 ((initialMap.Height + 1) / 2)

        let model = {
            Map = initialMap
            PlayerModel = playerModel
            VisibilityState = VisibilityState(initialMap.Width, initialMap.Height)
            VisibilityHalfWidth = visibilityHalfWidth
            VisibilityHalfHeight = visibilityHalfHeight
            VisibilityTranslucencyBudget = 0
            TileComplexStateInstance = Dictionary<int, ComplexState>() 
        }

        recomputeVisibility model
        model


    let create (initialMap: TileMap) : GameModel =
        createWith 0 initialMap NpcFrames.Default None

    let createAt spawnPointIndex (initialMap: TileMap) : GameModel =
        createWith spawnPointIndex initialMap NpcFrames.Default None

    let createEmpty (initialMap: TileMap) : GameModel =
        create initialMap
 
    let createWithPlayer (initialMap: TileMap) (playerFrames: NpcFrames) (playerActorId: int) : GameModel =
        createWith 0 initialMap playerFrames (Some playerActorId) 

    let setVisibilityWindow (model: GameModel) (halfWidth: int) (halfHeight: int) : unit =
        model.VisibilityHalfWidth <- max 0 halfWidth
        model.VisibilityHalfHeight <- max 0 halfHeight
        recomputeVisibility model

    let setVisibilityTranslucencyBudget (model: GameModel) (budget: int) : unit =
        model.VisibilityTranslucencyBudget <- max 0 budget
        recomputeVisibility model
        
    let lookAt (model: GameModel) (pos: GridPos) : LookAtInfo =
        let map = model.Map
        if not (Utils.inBounds map pos) then
            { Result = LookAtResult.Null
              LayerCount = 0uy
              HasMore = false
              LayerKey0 = ""
              LayerKey1 = ""
              LayerKey2 = ""
              BaseKey = "" }
        else 
            // Keep only the top 3 by (RenderLayer, StableOrder).
            // Highest RenderLayer draws last (top). For ties, higher StableOrder draws last (top).
            let mutable e0Id = -1
            let mutable e0Layer = System.Int32.MinValue
            let mutable e0Order = System.Int32.MinValue

            let mutable e1Id = -1
            let mutable e1Layer = System.Int32.MinValue
            let mutable e1Order = System.Int32.MinValue

            let mutable e2Id = -1
            let mutable e2Layer = System.Int32.MinValue
            let mutable e2Order = System.Int32.MinValue

            let inline better (layerA: int) (orderA: int) (layerB: int) (orderB: int) =
                layerA > layerB || (layerA = layerB && orderA > orderB)

            let inline pushCandidate (id: int) (stableOrder: int) =
                if id < 0 then ()
                else
                    let layer = Utils.tryGetRenderLayer id
                    if better layer stableOrder e0Layer e0Order then
                        e2Id <- e1Id; e2Layer <- e1Layer; e2Order <- e1Order
                        e1Id <- e0Id; e1Layer <- e0Layer; e1Order <- e0Order
                        e0Id <- id;   e0Layer <- layer;    e0Order <- stableOrder
                    elif better layer stableOrder e1Layer e1Order then
                        e2Id <- e1Id; e2Layer <- e1Layer; e2Order <- e1Order
                        e1Id <- id;   e1Layer <- layer;    e1Order <- stableOrder
                    elif better layer stableOrder e2Layer e2Order then
                        e2Id <- id;   e2Layer <- layer;    e2Order <- stableOrder

            let cell = map.GetLayerCell(pos.X, pos.Y)
            let baseKey =
                match map.TryGetTileDescriptionKey(pos.X, pos.Y) with
                | None -> ""
                | Some k -> k

            // Gather candidates with stableOrder similar to rendering code.
            // This makes lookAt reflect what's visually on top.
            let mutable stableOrder = 0

            match cell.FixtureId with
            | Some fid ->
                pushCandidate fid stableOrder
                stableOrder <- stableOrder + 1
            | None -> ()

            match cell.ActorId with
            | Some aid ->
                pushCandidate aid stableOrder
                stableOrder <- stableOrder + 1
            | None -> ()

            // Items can be many; only consider the top render slice (no allocations).
            let view = LayerQueries.GetRenderItemView cell
            let mutable it = view.Start
            let itEnd = view.Start + view.Count
            while it < itEnd do
                pushCandidate view.Items.[it] stableOrder
                stableOrder <- stableOrder + 1
                it <- it + 1

            // Decals are bottom->top; stableOrder preserves insertion order as a tiebreaker.
            let mutable di = 0
            while di < cell.DecalCount do
                pushCandidate cell.Decals.[di] stableOrder
                stableOrder <- stableOrder + 1
                di <- di + 1

            let k0 = if e0Id >= 0 then Utils.tryGetEntityDescKey e0Id else ""
            let k1 = if e1Id >= 0 then Utils.tryGetEntityDescKey e1Id else ""
            let k2 = if e2Id >= 0 then Utils.tryGetEntityDescKey e2Id else ""

            let count =
                (if System.String.IsNullOrEmpty(k0) then 0 else 1)
                + (if System.String.IsNullOrEmpty(k1) then 0 else 1)
                + (if System.String.IsNullOrEmpty(k2) then 0 else 1)

            // "HasMore" means there are more layer objects present than listed.
            // (We intentionally base this on presence, not on whether DescKey exists.)
            let totalLayerObjects =
                (if cell.FixtureId.IsSome then 1 else 0)
                + (if cell.ActorId.IsSome then 1 else 0)
                + cell.Items.Count
                + cell.DecalCount

            let hasMore = totalLayerObjects > count

            let result =
                if count = 0 && System.String.IsNullOrEmpty(baseKey) then LookAtResult.Null
                elif count = 0 then LookAtResult.SingleObject
                elif hasMore then LookAtResult.MultipleObjectsIncompleteList
                else LookAtResult.MultipleObjectsFullyListed

            { Result = result
              LayerCount = byte count
              HasMore = hasMore
              LayerKey0 = k0
              LayerKey1 = k1
              LayerKey2 = k2
              BaseKey = baseKey }

    let interactAtResult (model: GameModel) (pos: GridPos) (interaction: InteractionType) : InteractionResult =
        // Keep this thin: resolve what we hit, then delegate.
        match interaction with
        | InteractionType.NullAction -> InteractionResult.Nothing
        | InteractionType.DoorInteraction doorAction -> Doors.tryInteractDoorResult model pos doorAction

    let interactAt (model: GameModel) (pos: GridPos) (interaction: InteractionType)  =
        interactAtResult model pos interaction
        |> InteractResult.ofInteractionResult

    /// Infer a sensible interaction for the tile at `pos` and dispatch to `interactAt`.
    /// This preserves the ability to call `interactAt` directly with an explicit
    /// `InteractionType` while providing a convenience path for player-initiated
    /// interactions that should pick an appropriate action based on tile type.
    /// Auto-resolving interaction helper that accepts pre-fetched tile properties.
    /// Accepting `props` lets callers avoid a second property lookup.
    let interactAutoAtResult (model: GameModel) (playerPos: GridPos) (pos: GridPos) (props: TileProperties) : InteractionResult =
        let map = model.Map
        if not (Utils.inBounds map pos) then InteractionResult.Nothing
        else
            match props.TileType with
            | TileType.Door ->
                // Ignore doors if player is standing on the same tile
                if pos = playerPos then InteractionResult.Nothing
                else Doors.tryInteractDoorResult model pos DoorAction.OpenOrCloseDoor
            | _ -> InteractionResult.Nothing

    let interactAutoAt (model: GameModel) (playerPos: GridPos) (pos: GridPos) (props: TileProperties) : InteractResult =
        interactAutoAtResult model playerPos pos props
        |> InteractResult.ofInteractionResult

    /// Attempt to interact on behalf of the player.  Returns
    /// (visualUpdateNeeded, result).  Spatial tie-breaker order is:
    /// facing tile ▶ current tile ▶ north ▶ south ▶ behind.
    let tryForInteractionsResult (model: GameModel) : bool * InteractionResult =
        let p = model.PlayerModel
        let pos = p.PlayerPos
        let dx = if p.PlayerVisual.Facing = ActorFacing.Right then 1 else -1

        let candidates = [|
            GridPos(pos.X + dx, pos.Y)      // facing
            pos                               // underfoot
            GridPos(pos.X, pos.Y - 1)        // north
            GridPos(pos.X, pos.Y + 1)        // south
            GridPos(pos.X - dx, pos.Y)       // behind
        |]

        let mutable result = InteractionResult.Nothing
        let mutable found = false
        let mutable i = 0
        while i < candidates.Length && not found do
            let cand = candidates.[i]
            if Utils.inBounds model.Map cand then
                let props = model.Map.GetTileProperties(cand.X, cand.Y)
                if props.Interactable then
                    // Use the auto-resolving helper which will dispatch to the
                    // appropriate interaction handler (doors, etc.). 
                    let res = interactAutoAtResult model pos cand props
                    if res.Succeeded then
                        result <- res
                        found <- true
            i <- i + 1

        if found then
            if result.Changes.VisibilityInputChanged then
                recomputeVisibility model
            true, result
        else
            false, InteractionResult.Nothing

    /// Attempt to interact on behalf of the player. Returns
    /// (visualUpdateNeeded, result). Spatial tie-breaker order is:
    /// facing tile, current tile, north, south, behind.
    let tryForInteractions (model: GameModel) : bool * InteractResult =
        let succeeded, result = tryForInteractionsResult model
        succeeded, InteractResult.ofInteractionResult result
