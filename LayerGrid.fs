namespace AspectGameEngine

open System.Collections.Generic
  
[<Struct>]
type FixtureProperties =
  { BlocksMovement: bool
    Interactable: bool
    TileOpacity: TileOpacity }

[<Struct>]
type ActorProperties =
  { TileOpacity: TileOpacity }
 
[<Struct>]
type SpriteType =
  | Actor of ActorProp: ActorProperties
  | Fixture of FixtureProp: FixtureProperties
  | Item 
  | Decal 

[<Struct>]
type SpriteProperties =
  { Sprite: SpriteRef
    SpriteType: SpriteType }
 
// Simple registries (IDs are ints assigned externally)
module EntityRegistry =
    [<Literal>]
    let MaxItemsPerTile = 100

    [<Literal>]
    let MaxRenderItemsPerTile = 10

    // let ItemProps = Dictionary<int, ItemProperties>()
    // let FixtureProps = Dictionary<int, FixtureProperties>()
    // let ActorProps = Dictionary<int, ActorProperties>()
    // let DecalProps = Dictionary<int, SpriteRef>()
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
            | SpriteType.Item
            | SpriteType.Decal -> None

// One logical "layer cell" per tile
type LayerCell =
    { mutable Items: ResizeArray<int> // item entity IDs
      mutable FixtureId: int option // fixture entity ID
      mutable ActorId: int option   // actor entity ID
      mutable DecalId: int option } // decal entity ID

    static member Create() =
        { Items = ResizeArray()
          FixtureId = None
          ActorId = None
          DecalId = None }

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
            | Some id ->
                match SpritePropsQueries.tryGetOpacity id with
                | Some TileOpacity.Opaque -> Some TileOpacity.Opaque
                | _ -> None

        match getOpacity cell.ActorId with
        | Some o -> o
        | None ->
            match getOpacity cell.FixtureId with
            | Some o -> o
            | None -> baseTileOpacity

//==============

// Immutable editor-side layer cell: items as immutable F# list<int>
type EditorLayerCell =
    { Items: int list
      FixtureId: int option
      ActorId: int option
      DecalId: int option }

    static member Empty =
        { Items = []
          FixtureId = None
          ActorId = None
          DecalId = None }

// Editor-side queries (immutable), separate from runtime LayerQueries
module EditorLayerQueries =
    // Return up to top N items (treat list tail as "top")
    let GetRenderItems (cell: EditorLayerCell) : int list =
        cell.Items |> List.truncate EntityRegistry.MaxRenderItemsPerTile |> List.rev

    // Items never affect opacity; only actor/fixture vs base tile.
    let EffectiveTileOpacity (baseTileOpacity: TileOpacity, cell: EditorLayerCell) =
        let inline getOpacity (spriteIdOpt:int option) =
            match spriteIdOpt with
            | None -> None
            | Some id ->
                match SpritePropsQueries.tryGetOpacity id with
                | Some TileOpacity.Opaque -> Some TileOpacity.Opaque
                | _ -> None

        match getOpacity cell.ActorId with
        | Some o -> o
        | None ->
            match getOpacity cell.FixtureId with
            | Some o -> o
            | None -> baseTileOpacity 

// Replace EditorLayerCellUpdate to target the immutable editor cell
type EditorLayerCellUpdate =
    { X: int
      Y: int
      Cell: EditorLayerCell }
