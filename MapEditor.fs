namespace AspectGameEngine

open AspectGameEngine
open FSharpx.Collections 

type EditorTileMap =
    { Width: int
      Height: int
      Tiles: PersistentVector<Tile> // Single flat vector
      VoidSpriteLoc: SpriteLoc
      MapName: string
      MapType: MapType
      TilesetName: string
      LayerCells: PersistentVector<EditorLayerCell> }

    // Helper to calculate 1D index from 2D coordinates.
    // Internal, assumes x, y, currentWidth are valid for the context.
    member private this.GetFlatIndexUnchecked(x, y, currentWidth) = y * currentWidth + x

    // Public helper for current map dimensions, with bounds checking.
    member private this.GetFlatIndex(x, y) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            failwithf "Coordinates (%d, %d) are out of bounds for map size (%d, %d)" x y this.Width this.Height

        this.GetFlatIndexUnchecked(x, y, this.Width)

    member this.GetLayerCell(x, y) =
        let index = this.GetFlatIndex(x, y)
        this.LayerCells.[index]

    static member New(width, height, voidSpriteLoc, tileSet, ?mapName, ?mapType) =
        if width <= 0 || height <= 0 then
            // Enforce positive dimensions for a valid initial map.
            // Resize can handle 0 dimensions if needed for intermediate states.
            failwith "Width and height must be positive for a new map."

        let size = width * height
        { Width = width
          Height = height
          Tiles =
            PersistentVector.init size (fun _ ->
                { SpriteLoc = voidSpriteLoc
                  Health = 0
                  IsOccupied = false })
          VoidSpriteLoc = voidSpriteLoc
          MapName = defaultArg mapName ""
          MapType = defaultArg mapType MapType.Room
          TilesetName = tileSet
          LayerCells = PersistentVector.init size (fun _ -> EditorLayerCell.Empty) }

    member this.GetTile(x, y) =
        let index = this.GetFlatIndex(x, y)
        this.Tiles.[index]

    member this.GetTileProperties(x, y) =
        TilesetRegistry.get(this.TilesetName)[this.GetTile(x, y).SpriteLoc]

    member this.GetActor(x, y) =
        this.GetLayerCell(x, y).ActorId

    member this.GetFixture(x, y) =
        this.GetLayerCell(x, y).FixtureId

    member this.GetDecal(x, y) =
        this.GetLayerCell(x, y).DecalId

    member this.SetActor(x, y, actorId) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with ActorId = Some actorId }) }

    member this.ClearActor(x, y) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with ActorId = None }) }

    member this.SetFixture(x, y, fixtureId) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with FixtureId = Some fixtureId }) }

    member this.ClearFixture(x, y) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with FixtureId = None }) }

    member this.SetDecal(x, y, decalId) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with DecalId = Some decalId }) }

    member this.ClearDecal(x, y) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        { this with
            LayerCells = this.LayerCells.Update(index, { cell with DecalId = None }) }

    member this.SetEntityAuto(x: int, y: int, entityId: int) =
        match SpritePropsQueries.tryGet entityId with
        | None ->
            // If the id isn't registered, do nothing (alternatively: failwithf).
            this
        | Some sp ->
            match sp.SpriteType with
            | SpriteType.Actor _ -> this.SetActor(x, y, entityId)
            | SpriteType.Fixture _ -> this.SetFixture(x, y, entityId)
            | SpriteType.Decal -> this.SetDecal(x, y, entityId)
            | SpriteType.Item -> this.AddItem(x, y, entityId)

    member this.MigrateEntitySpriteType(entityId: int, oldType: SpriteType, newType: SpriteType) =
        let kindOf (t: SpriteType) =
            match t with
            | SpriteType.Actor _ -> 0
            | SpriteType.Fixture _ -> 1
            | SpriteType.Decal -> 2
            | SpriteType.Item -> 3

        let oldKind = kindOf oldType
        let newKind = kindOf newType

        // No-op if the “container kind” didn’t change.
        if oldKind = newKind then
            this
        else
            // Rule: if we cross the Item boundary, we destroy placement (remove from all cells).
            let crossesItemBoundary =
                (oldKind = 3 && newKind <> 3) || (oldKind <> 3 && newKind = 3)

            let migrateCell (cell: EditorLayerCell) =
                let mutable changed = false

                // Always remove from items if present (safety + supports destroy/migrate rules)
                let hadItem = cell.Items |> List.exists (fun id -> id = entityId)
                let items2 =
                    if hadItem then
                        changed <- true
                        cell.Items |> List.filter (fun id -> id <> entityId)
                    else
                        cell.Items

                let inline isMe v = (v = Some entityId)

                let a0 = if isMe cell.ActorId then changed <- true; None else cell.ActorId
                let f0 = if isMe cell.FixtureId then changed <- true; None else cell.FixtureId
                let d0 = if isMe cell.DecalId then changed <- true; None else cell.DecalId

                if crossesItemBoundary then
                    // “Destroy”: entityId is removed everywhere; no reinsert.
                    { cell with Items = items2; ActorId = a0; FixtureId = f0; DecalId = d0 }, changed
                else
                    // Non-item -> non-item: migrate into the new slot if it is empty (None); otherwise destroy.
                    let wasPresentInSlot = isMe cell.ActorId || isMe cell.FixtureId || isMe cell.DecalId

                    if not wasPresentInSlot then
                        { cell with Items = items2 }, changed
                    else
                        // Destination must be empty (None) to accept the migrated id.
                        let a1, f1, d1 =
                            match newKind with
                            | 0 -> // Actor
                                if a0.IsNone then (Some entityId, f0, d0) else (a0, f0, d0)
                            | 1 -> // Fixture
                                if f0.IsNone then (a0, Some entityId, d0) else (a0, f0, d0)
                            | 2 -> // Decal
                                if d0.IsNone then (a0, f0, Some entityId) else (a0, f0, d0)
                            | _ ->
                                // Shouldn’t happen here (items excluded)
                                (a0, f0, d0)

                        // If destination was blocked, we already cleared the old slot => “destroy”
                        { cell with Items = items2; ActorId = a1; FixtureId = f1; DecalId = d1 }, true

            // Apply across all cells
            let updates =
                [|
                    for i = 0 to this.LayerCells.Length - 1 do
                        let cell = this.LayerCells.[i]
                        let cell2, changed = migrateCell cell
                        if changed then
                            yield (i, cell2)
                |]
            
            if updates.Length = 0 then this
            else { this with LayerCells = PersistentVector.updateMany updates this.LayerCells }

    member this.UpdateLayerCell(x: int, y: int, cell: EditorLayerCell) =
        let index = this.GetFlatIndex(x, y)
        { this with LayerCells = this.LayerCells.Update(index, cell) }
 
    member this.UpdateLayerCells(updates: EditorLayerCellUpdate[]) =
        if Array.isEmpty updates then this
        else
            let mapper (u: EditorLayerCellUpdate) =
                let index = this.GetFlatIndex(u.X, u.Y)
                (index, u.Cell)
            { this with LayerCells = PersistentVector.updateManyWith mapper updates this.LayerCells }

    member this.AddItem(x, y, itemId) =
        let index = this.GetFlatIndex(x, y)
        let cell = this.LayerCells.[index]
        if List.length cell.Items < EntityRegistry.MaxItemsPerTile then
            { this with
                LayerCells = this.LayerCells.Update(index, { cell with Items = itemId :: cell.Items }) }
        else
            this

    member this.IsWalkable(x, y) = this.GetTileProperties(x, y).Walkable 

    member this.IsOccupied(x, y) =
        let tile = this.GetTile(x, y)
        let cell = this.GetLayerCell(x, y)
        if tile.IsOccupied then true
        else
            match cell.ActorId with
            | Some _ -> true
            | None ->
                match cell.FixtureId with
                | None -> false
                | Some fid ->
                    Option.map (fun sp -> SpritePropsQueries.checkFixtureBlocksMovement sp.SpriteType) (SpritePropsQueries.tryGet fid)
                    |> Option.defaultValue false

    member this.IsOpaque(x, y) =
        let baseOpacity = this.GetTileProperties(x, y).TileOpacity
        let cell = this.GetLayerCell(x, y)
        match EditorLayerQueries.EffectiveTileOpacity(baseOpacity, cell) with 
        | TileOpacity.Opaque -> true
        | _ -> false

    member this.UpdateName name = { this with MapName = name }
    member this.UpdateMapType mapType = { this with MapType = mapType }

    member this.UpdateVoidSpriteLoc voidSpriteLoc =
        { this with
            VoidSpriteLoc = voidSpriteLoc }

    member this.UpdateTile(x, y, tile) =
        let index = this.GetFlatIndex(x, y)

        { this with
            Tiles = this.Tiles.Update(index, tile) }

    member this.UpdateWidth width = this.Resize(width, this.Height)
    member this.UpdateHeight height = this.Resize(this.Width, height)

    member this.Resize(newWidth, newHeight) =
        if newWidth < 0 || newHeight < 0 then
            failwith "New width and height cannot be negative."

        let oldTiles = this.Tiles
        let oldLayers = this.LayerCells
        let oldWidth = this.Width
        let oldHeight = this.Height
 
        if newWidth = oldWidth && newHeight = oldHeight then
            this
        else
            let emptyTile =
                { SpriteLoc = this.VoidSpriteLoc
                  Health = 0
                  IsOccupied = false }
            let emptyCell = EditorLayerCell.Empty

            // Handle cases where the new map is effectively empty.
            if newWidth = 0 || newHeight = 0 then
                { this with
                    Width = newWidth
                    Height = newHeight
                    Tiles = PersistentVector.empty
                    LayerCells = PersistentVector.empty }
            // Fast path: Only height changes, width remains the same.
            // This can be done by taking a prefix of oldTiles or appending new blank rows.
            elif newWidth = oldWidth then
                let finalTiles, finalLayers =
                    if newHeight > oldHeight then // Grow taller
                        let additionalRowsCount = newHeight - oldHeight
                        // Create a single vector representing all new blank tiles to append
                        let newBlankTiles =
                            PersistentVector.init (additionalRowsCount * newWidth) (fun _ -> emptyTile)
                        let newBlankCells =
                            PersistentVector.init (additionalRowsCount * newWidth) (fun _ -> emptyCell)

                        PersistentVector.append oldTiles newBlankTiles,
                        PersistentVector.append oldLayers newBlankCells
                    else // newHeight < oldHeight, Shrink shorter
                        PersistentVector.take (newWidth * newHeight) oldTiles,
                        PersistentVector.take (newWidth * newHeight) oldLayers

                { this with
                    Height = newHeight
                    Tiles = finalTiles
                    LayerCells = finalLayers }
            // General case: Width changes (or both width and height change).
            else
                let newRowsSeq =
                    seq {
                        for ny in 0 .. newHeight - 1 do
                            if ny < oldHeight && oldWidth > 0 then
                                // This new row (indexed by ny) has some correspondence with an old row.
                                // Extract the data for the ny-th row from oldTiles.
                                // This segment has 'oldWidth' elements.
                                let oldRowFlatStartIndex = ny * oldWidth

                                let rowDataFromOldTiles =
                                    PersistentVector.take oldWidth (PersistentVector.skip oldRowFlatStartIndex oldTiles)
                                let rowDataFromOldLayers =
                                    PersistentVector.take oldWidth (PersistentVector.skip oldRowFlatStartIndex oldLayers)
                                
                                // Now adjust this rowDataFromOldTiles (which is of length oldWidth) to newWidth.
                                if newWidth = oldWidth then // Width of this row segment is already correct (should be covered by fast path but defensive).
                                    yield (rowDataFromOldTiles, rowDataFromOldLayers)
                                elif newWidth < oldWidth then // Shrink width for this row.
                                    yield (PersistentVector.take newWidth rowDataFromOldTiles,
                                           PersistentVector.take newWidth rowDataFromOldLayers)
                                else // Expand width for this row (newWidth > oldWidth).
                                    let paddingCount = newWidth - oldWidth
                                    let tilePadding = PersistentVector.init paddingCount (fun _ -> emptyTile)
                                    let cellPadding = PersistentVector.init paddingCount (fun _ -> emptyCell)
                                    yield (PersistentVector.append rowDataFromOldTiles tilePadding,
                                           PersistentVector.append rowDataFromOldLayers cellPadding)
                            else
                                // This is a completely new row (either ny >= oldHeight, or the old map had oldWidth=0).
                                // So, fill it entirely with empty tiles for the newWidth.
                                yield (PersistentVector.init newWidth (fun _ -> emptyTile),
                                       PersistentVector.init newWidth (fun _ -> emptyCell))
                    }

                let finalTiles = PersistentVector.concat (newRowsSeq |> Seq.map fst)
                let finalLayers = PersistentVector.concat (newRowsSeq |> Seq.map snd)

                { this with
                    Width = newWidth
                    Height = newHeight
                    Tiles = finalTiles
                    LayerCells = finalLayers }

    member this.BatchUpdate(updates: TileUpdate[]) =
        if Array.isEmpty updates then
            this
        else
            let mapper (update: TileUpdate) =
                let index = this.GetFlatIndex(update.X, update.Y)
                (index, update.Tile)

            { this with
                Tiles = PersistentVector.updateManyWith mapper updates this.Tiles }

    // Convert from runtime TileMap to editor EditorTileMap
    static member FromTileMap(tileMap: TileMap) =
        let tilesVector = PersistentVector.ofSeq tileMap.Tiles
         
        let layersVector = 
            tileMap.LayerCells 
            |> Array.map (fun cell -> 
                { Items = List.ofSeq cell.Items
                  FixtureId = cell.FixtureId
                  DecalId = cell.DecalId
                  ActorId = cell.ActorId })
            |> PersistentVector.ofSeq
        
        { Width = tileMap.Width
          Height = tileMap.Height
          Tiles = tilesVector
          VoidSpriteLoc = tileMap.VoidSpriteLoc
          MapName = tileMap.MapName
          MapType = tileMap.MapType
          TilesetName = tileMap.TileSetName
          LayerCells = layersVector }

    // Convert from editor EditorTileMap to runtime TileMap
    member this.ToTileMap() =         
        let tilesArray = this.Tiles |> PersistentVector.toArray
         
        let layersArray = 
            this.LayerCells 
            |> PersistentVector.toArray
            |> Array.map (fun cell ->
                { LayerCell.Items = ResizeArray(cell.Items)
                  FixtureId = cell.FixtureId
                  DecalId = cell.DecalId
                  ActorId = cell.ActorId })
        
        TileMap(
            this.Width,
            this.Height,
            tilesArray,
            layersArray,
            this.VoidSpriteLoc,
            this.TilesetName,
            this.MapName,
            this.MapType
        ) 

type EditorHistory =
    { History: List<EditorTileMap>
      CurrentIndex: int
      MaxHistorySize: int }

    member this.HistorySize = this.History.Length
    member this.CanUndo = this.CurrentIndex < this.History.Length - 1
    member this.CanRedo = this.CurrentIndex > 0

    static member New(maxHistorySize) =
        { History = []
          CurrentIndex = 0
          MaxHistorySize = maxHistorySize }

    member this.UpdateTile(x: int, y: int, tile: Tile) =
        let current = this.History[this.CurrentIndex].UpdateTile(x, y, tile)
        this.AddTileMap current

    member this.UpdateTiles(updates: TileUpdate[]) =
        let current = this.History[this.CurrentIndex].BatchUpdate(updates)
        this.AddTileMap current

    member this.Resize(width: int, height: int) =
        this.CurrentTileMap.Resize(width, height) |> this.AddTileMap

    member this.UpdateWidth width =
        this.Resize(width, this.CurrentTileMap.Height)

    member this.UpdateHeight height =
        this.Resize(this.CurrentTileMap.Width, height)

    member this.CurrentTileMap: EditorTileMap = this.History[this.CurrentIndex]

    member this.Redo() =
        if this.CurrentIndex > 0 then
            { this with
                CurrentIndex = this.CurrentIndex - 1 }
        else
            this

    member this.Undo() =
        if this.CurrentIndex < this.History.Length - 1 then
            { this with
                CurrentIndex = this.CurrentIndex + 1 }
        else
            this

    member this.SetActor(x:int, y:int, actorId:int) =
        let current = this.CurrentTileMap.SetActor(x, y, actorId)
        this.AddTileMap current

    member this.ClearActor(x:int, y:int) =
        let current = this.CurrentTileMap.ClearActor(x, y)
        this.AddTileMap current

    member this.SetFixture(x:int, y:int, fixtureId:int) =
        let current = this.CurrentTileMap.SetFixture(x, y, fixtureId)
        this.AddTileMap current

    member this.SetEntityAuto(x: int, y: int, entityId: int) =
        this.CurrentTileMap.SetEntityAuto(x, y, entityId)
        |> this.AddTileMap

    member this.ClearFixture(x:int, y:int) =
        let current = this.CurrentTileMap.ClearFixture(x, y)
        this.AddTileMap current

    member this.SetDecal(x:int, y:int, decalId:int) =
        let current = this.CurrentTileMap.SetDecal(x, y, decalId)
        this.AddTileMap current

    member this.ClearDecal(x:int, y:int) =
        let current = this.CurrentTileMap.ClearDecal(x, y)
        this.AddTileMap current

    member this.AddItem(x:int, y:int, itemId:int) =
        let current = this.CurrentTileMap.AddItem(x, y, itemId)
        this.AddTileMap current

    member this.UpdateLayerCell(x: int, y: int, cell: EditorLayerCell) =  
        this.CurrentTileMap.UpdateLayerCell(x, y, cell) |> this.AddTileMap

    member this.UpdateLayerCells(updates: EditorLayerCellUpdate[]) =
        this.CurrentTileMap.UpdateLayerCells(updates) |> this.AddTileMap

    member this.AddTileMap(tileMap: EditorTileMap) : EditorHistory =
        let newHistory =
            if this.HistorySize = this.MaxHistorySize then
                List.rev (List.tail (List.rev this.History))
            else
                this.History

        if this.CurrentIndex <> 0 then
            let _, truncatedList = List.splitAt this.CurrentIndex newHistory

            { this with
                History = tileMap :: truncatedList
                CurrentIndex = 0 }
        else
            { this with
                History = tileMap :: newHistory }
