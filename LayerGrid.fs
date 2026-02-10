namespace AspectGameEngine

open System.Collections.Generic
  
[<Struct>]
type FixtureProperties =
  { BlocksMovement: bool
    Interactable: bool
    DescKey: string
    TileOpacity: TileOpacity }

[<Struct>]
type ActorProperties =
  { TileOpacity: TileOpacity 
    DescKey: string }

[<Struct>]  
type DecalProperties =
  {Interactable: bool
   DescKey: string }

[<Struct>]
type ItemProperties =
  { DescKey: string }
 
[<Struct>]
type SpriteType =
  | Actor of ActorProp: ActorProperties
  | Fixture of FixtureProp: FixtureProperties
  | Item of ItemProp: ItemProperties
  | Decal of DecalProp: DecalProperties

[<Struct>]
type SpriteProperties =
  { Sprite: SpriteRef
    SpriteType: SpriteType
    RenderLayer: int }
 
// Simple registries (IDs are ints assigned externally)
module EntityRegistry =
    [<Literal>]
    let MaxItemsPerTile = 100

    [<Literal>]
    let MaxRenderItemsPerTile = 10

    [<Literal>]
    let MaxDecalsPerTile = 5

    let SpriteProps = Dictionary<int,SpriteProperties>() 


module SpritePropsQueries =
    let inline tryGet (id:int) =
        match EntityRegistry.SpriteProps.TryGetValue id with
        | true, sp -> Some sp
        | _ -> None 

    let checkFixtureBlocksMovement = function 
        | SpriteType.Fixture fp -> fp.BlocksMovement
        | _ -> false 

    let tryGetBlocksMovementFromId (id:int) =
        match tryGet id with
        | Some sp ->
            match sp.SpriteType with
            | SpriteType.Actor _ -> true
            | SpriteType.Fixture fp -> fp.BlocksMovement
            | _ -> false
        | None -> false

    let getBlocksMovememntFromSpriteType = function
        | SpriteType.Actor _ -> true
        | SpriteType.Fixture fp -> fp.BlocksMovement
        | _ -> false 

    let tryGetOpacity (id:int) : TileOpacity option =
        match tryGet id with
        | None -> None
        | Some sp ->
            match sp.SpriteType with
            | SpriteType.Actor ap -> Some ap.TileOpacity
            | SpriteType.Fixture fp -> Some fp.TileOpacity
            | SpriteType.Item _
            | SpriteType.Decal _ -> None

// One logical "layer cell" per tile
type LayerCell =
    { mutable Items: ResizeArray<int> // item entity IDs
      mutable FixtureId: int option // fixture entity ID
      mutable ActorId: int option   // actor entity ID
      mutable Decals: int[] // bottom->top; may be null when empty
      mutable DecalCount: int } // number of active decals

    static member Create() =
        { Items = ResizeArray()
          FixtureId = None
          ActorId = None
          Decals = [||]
          DecalCount = 0 }

// Non-allocating view over the items to render (top N).
[<Struct>]
type ItemView =
    { Items: ResizeArray<int>
      Start: int
      Count: int }

module LayerQueries =
    // Return a slice view (no allocations) for the top N render items.
    let GetRenderItemView (cell: LayerCell) : ItemView =
        let count = cell.Items.Count

        if count = 0 then
            { Items = cell.Items
              Start = 0
              Count = 0 }
        else
            let take =
                if count > EntityRegistry.MaxRenderItemsPerTile then
                    EntityRegistry.MaxRenderItemsPerTile
                else
                    count

            let start = count - take

            { Items = cell.Items
              Start = start
              Count = take }

    // Copy top N render items into a caller-provided buffer. Returns copied count.
    let CopyRenderItemIds (cell: LayerCell, dst: int[], dstStart: int) : int =
        let view = GetRenderItemView cell
        let mutable i = 0

        while i < view.Count do
            dst.[dstStart + i] <- view.Items.[view.Start + i]
            i <- i + 1

        view.Count

    // Items never affect opacity; only actor/fixture vs base tile.
    let EffectiveTileOpacity (baseTileOpacity: TileOpacity, cell: LayerCell) =
        let inline getOpacity (spriteIdOpt:int option) =
            match spriteIdOpt with
            | None -> None
            | Some id -> SpritePropsQueries.tryGetOpacity id

        match getOpacity cell.ActorId with
        | Some o -> o
        | None ->
            match getOpacity cell.FixtureId with
            | Some o -> o
            | None -> baseTileOpacity

    [<Struct>]
    type DecalView =
        { Decals: int[]
          Count: int }

    let GetDecalView (cell: LayerCell) : DecalView =
        if cell.DecalCount <= 0 || isNull cell.Decals || cell.Decals.Length = 0 then
            { Decals = Array.empty
              Count = 0 }
        else
            { Decals = cell.Decals
              Count = cell.DecalCount }

    let CopyDecalIds (cell: LayerCell, dst: int[], dstStart: int) : int =
        let view = GetDecalView cell
        let mutable i = 0
        while i < view.Count do
            dst.[dstStart + i] <- view.Decals.[i]
            i <- i + 1
        view.Count

    let TryGetTopDecalId (cell: LayerCell) : int option =
        if cell.DecalCount <= 0 || isNull cell.Decals || cell.Decals.Length = 0 then None
        else Some cell.Decals.[cell.DecalCount - 1]

    let ClearDecals (cell: LayerCell) : unit =
        cell.DecalCount <- 0

    let AddDecal (cell: LayerCell, decalId: int) : unit =
        // Fixed-capacity stack: newest is topmost.
        if isNull cell.Decals || cell.Decals.Length = 0 then
            cell.Decals <- Array.zeroCreate EntityRegistry.MaxDecalsPerTile
            cell.DecalCount <- 0

        if cell.DecalCount < EntityRegistry.MaxDecalsPerTile then
            cell.Decals.[cell.DecalCount] <- decalId
            cell.DecalCount <- cell.DecalCount + 1
        else
            // When full, replace the current topmost decal.
            cell.Decals.[EntityRegistry.MaxDecalsPerTile - 1] <- decalId

//==============

// Immutable editor-side layer cell: items as immutable F# list<int>
type EditorLayerCell =
    { Items: int list
      FixtureId: int option
      ActorId: int option
      Decals: int list }

    static member Empty =
        { Items = []
          FixtureId = None
          ActorId = None
          Decals = [] }

    member this.UpdateFixtureId(newId: int option) =
        { this with FixtureId = newId }
    
    member this.UpdateActorId(newId: int option) =
        { this with ActorId = newId }
    member this.AddDecalToTopUpdate(decalId: int) =
        if List.length this.Decals >= EntityRegistry.MaxDecalsPerTile then
            { this with Decals = decalId :: this.Decals.Tail }
        else
            { this with Decals = decalId :: this.Decals }

// Editor-side queries (immutable), separate from runtime LayerQueries
module EditorLayerQueries =
    // Return up to top N items (list head is topmost/newest)
    let GetRenderItems (cell: EditorLayerCell) : int list =
        cell.Items |> List.truncate EntityRegistry.MaxRenderItemsPerTile

    // Return up to top N decals (list head is topmost/newest)
    let GetRenderDecals (cell: EditorLayerCell) : int list =
        cell.Decals |> List.truncate EntityRegistry.MaxDecalsPerTile

    // Items never affect opacity; only actor/fixture vs base tile.
    let EffectiveTileOpacity (baseTileOpacity: TileOpacity, cell: EditorLayerCell) =
        let inline getOpacity (spriteIdOpt:int option) = 
            Option.bind SpritePropsQueries.tryGetOpacity spriteIdOpt

        match getOpacity cell.ActorId with
        | Some o -> o
        | None ->
            match getOpacity cell.FixtureId with
            | Some o -> o
            | None -> baseTileOpacity 

/// Replace EditorLayerCellUpdate to target the immutable editor cell
type EditorLayerCellUpdate =
    { X: int
      Y: int
      Cell: EditorLayerCell }
