namespace AspectGameEngine

open System.Collections.Generic

[<Struct>]
type ItemProperties =
    { Sprite: SpriteRef
      // Items never block light: enforce Transparent/Air only.
      TileOpacity: TileOpacity } 

[<Struct>]
type FixtureProperties =
    { Sprite: SpriteRef
      BlocksMovement: bool 
      Interactable: bool
      TileOpacity: TileOpacity }

[<Struct>]
type ActorProperties =
    { Sprite: SpriteRef
      BlocksMovement: bool
      TileOpacity: TileOpacity }

// Simple registries (IDs are ints assigned externally)
module EntityRegistry =
    [<Literal>]
    let MaxItemsPerTile = 100

    [<Literal>]
    let MaxRenderItemsPerTile = 10

    let ItemProps    = Dictionary<int, ItemProperties>()
    let FixtureProps = Dictionary<int, FixtureProperties>()
    let ActorProps   = Dictionary<int, ActorProperties>()
    let DecalProps   = Dictionary<int, SpriteRef>()

// One logical "layer cell" per tile
type LayerCell =
    { mutable Items: ResizeArray<int>         // item entity IDs
      mutable FixtureId: int option           // fixture entity ID
      mutable ActorId: int option            // actor entity ID
      mutable DecalId: int option }          // decal entity ID

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
            { Items = cell.Items; Start = 0; Count = 0 }
        else
            let take = if count > EntityRegistry.MaxRenderItemsPerTile then EntityRegistry.MaxRenderItemsPerTile else count
            let start = count - take
            { Items = cell.Items; Start = start; Count = take }

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
        match cell.ActorId with
        | Some aId ->
            match EntityRegistry.ActorProps.TryGetValue aId with
            | true, ap when ap.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
            | _ ->
                match cell.FixtureId with
                | Some fId ->
                    match EntityRegistry.FixtureProps.TryGetValue fId with
                    | true, fp when fp.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
                    | _ -> baseTileOpacity
                | None -> baseTileOpacity
        | None ->
            match cell.FixtureId with
            | Some fId ->
                match EntityRegistry.FixtureProps.TryGetValue fId with
                | true, fp when fp.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
                | _ -> baseTileOpacity
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
         cell.Items
        |> List.truncate EntityRegistry.MaxRenderItemsPerTile
        |> List.rev
        
    // Items never affect opacity; only actor/fixture vs base tile.
    let EffectiveTileOpacity (baseTileOpacity: TileOpacity, cell: EditorLayerCell) =
        match cell.ActorId with
        | Some aId ->
            match EntityRegistry.ActorProps.TryGetValue aId with
            | true, ap when ap.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
            | _ ->
                match cell.FixtureId with
                | Some fId ->
                    match EntityRegistry.FixtureProps.TryGetValue fId with
                    | true, fp when fp.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
                    | _ -> baseTileOpacity
                | None -> baseTileOpacity
        | None ->
            match cell.FixtureId with
            | Some fId ->
                match EntityRegistry.FixtureProps.TryGetValue fId with
                | true, fp when fp.TileOpacity = TileOpacity.Opaque -> TileOpacity.Opaque
                | _ -> baseTileOpacity
            | None -> baseTileOpacity

// Replace EditorLayerCellUpdate to target the immutable editor cell
type EditorLayerCellUpdate =
    { X: int
      Y: int
      Cell: EditorLayerCell }