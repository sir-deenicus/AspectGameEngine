namespace AspectGameEngine

open AspectGameEngine
open FSharpx.Collections

type TilePropertiesImmutableReference =
    private
        { Properties: Map<SpriteLoc, TileProperties>
          TileSetName: string }

    static member New(tileSetName) =
        { Properties = Map.empty
          TileSetName = tileSetName }

    // Read-only indexer to get properties
    member this.Item
        with get spriteLoc =
            match this.Properties.TryFind spriteLoc with
            | Some tile -> tile
            | None -> TileProperties.NullTile

    member this.GetTileSetName() = this.TileSetName

    member this.SetTileSetName(tileSetName) = { this with TileSetName = tileSetName }

    // Method to set or update a tile property, returning a new instance
    member this.Set(spriteLoc: SpriteLoc, value: TileProperties) =
        { this with
            Properties = this.Properties.Add(spriteLoc, value) }

    // The original 'Update' method, now also returning a new instance
    member this.Update(spriteLoc: SpriteLoc, tileProperties: TileProperties) =
        // Map.Add will add the key-value pair, or update the value if the key already exists.
        { this with
            Properties = this.Properties.Add(spriteLoc, tileProperties) }


type EditorTileMap =
    { Width: int
      Height: int
      Tiles: PersistentVector<Tile> // Single flat vector
      VoidSpriteLoc: SpriteLoc
      MapName: string
      MapType: MapType
      TilePropertiesReference: TilePropertiesImmutableReference }

    // Helper to calculate 1D index from 2D coordinates.
    // Internal, assumes x, y, currentWidth are valid for the context.
    member private this.GetFlatIndexUnchecked(x, y, currentWidth) = y * currentWidth + x

    // Public helper for current map dimensions, with bounds checking.
    member private this.GetFlatIndex(x, y) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            failwithf "Coordinates (%d, %d) are out of bounds for map size (%d, %d)" x y this.Width this.Height

        this.GetFlatIndexUnchecked(x, y, this.Width)

    static member New(width, height, voidSpriteLoc, tilePropertiesReference, ?mapName, ?mapType) =
        if width <= 0 || height <= 0 then
            // Enforce positive dimensions for a valid initial map.
            // Resize can handle 0 dimensions if needed for intermediate states.
            failwith "Width and height must be positive for a new map."

        { Width = width
          Height = height
          Tiles =
            PersistentVector.init (width * height) (fun _ ->
                { SpriteLoc = voidSpriteLoc
                  Health = 0
                  IsOccupied = false })
          VoidSpriteLoc = voidSpriteLoc
          MapName = defaultArg mapName ""
          MapType = defaultArg mapType MapType.Room
          TilePropertiesReference = tilePropertiesReference }

    member this.GetTile(x, y) =
        let index = this.GetFlatIndex(x, y)
        this.Tiles.[index]

    member this.GetTileProperties(x, y) =
        this.TilePropertiesReference.[this.GetTile(x, y).SpriteLoc]

    member this.IsWalkable(x, y) = this.GetTileProperties(x, y).Walkable
    member this.IsVoid(x, y) = this.GetTileProperties(x, y).IsVoid

    member this.IsOpaque(x, y) =
        this.GetTileProperties(x, y).TileOpacity = TileOpacity.Opaque

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
        let oldWidth = this.Width
        let oldHeight = this.Height

        // If dimensions are unchanged, no work needed.
        if newWidth = oldWidth && newHeight = oldHeight then
            this
        else
            let emptyTile =
                { SpriteLoc = this.VoidSpriteLoc
                  Health = 0
                  IsOccupied = false }

            // Handle cases where the new map is effectively empty.
            if newWidth = 0 || newHeight = 0 then
                { this with
                    Width = newWidth
                    Height = newHeight
                    Tiles = PersistentVector.empty }
            // Fast path: Only height changes, width remains the same.
            // This can be done by taking a prefix of oldTiles or appending new blank rows.
            elif newWidth = oldWidth then
                let finalTiles =
                    if newHeight > oldHeight then // Grow taller
                        let additionalRowsCount = newHeight - oldHeight
                        // Create a single vector representing all new blank tiles to append
                        let newBlankTiles =
                            PersistentVector.init (additionalRowsCount * newWidth) (fun _ -> emptyTile)

                        PersistentVector.append oldTiles newBlankTiles
                    else // newHeight < oldHeight, Shrink shorter
                        PersistentVector.take (newWidth * newHeight) oldTiles

                { this with
                    Height = newHeight
                    Tiles = finalTiles }
            // General case: Width changes (or both width and height change).
            // Reconstruct row by row, using take/skip/append for structural sharing.
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

                                // Now adjust this rowDataFromOldTiles (which is of length oldWidth) to newWidth.
                                if newWidth = oldWidth then // Width of this row segment is already correct (should be covered by fast path but defensive).
                                    yield rowDataFromOldTiles
                                elif newWidth < oldWidth then // Shrink width for this row.
                                    yield PersistentVector.take newWidth rowDataFromOldTiles
                                else // Expand width for this row (newWidth > oldWidth).
                                    let paddingCount = newWidth - oldWidth
                                    let padding = PersistentVector.init paddingCount (fun _ -> emptyTile)
                                    yield PersistentVector.append rowDataFromOldTiles padding
                            else
                                // This is a completely new row (either ny >= oldHeight, or the old map had oldWidth=0).
                                // So, fill it entirely with empty tiles for the newWidth.
                                yield PersistentVector.init newWidth (fun _ -> emptyTile)
                    }

                // Concatenate all the generated new row vectors.
                // PersistentVector.concat is designed to efficiently combine persistent vectors.
                let finalTiles = PersistentVector.concat newRowsSeq

                { this with
                    Width = newWidth
                    Height = newHeight
                    Tiles = finalTiles }

    member this.BatchUpdate(updates: TileUpdate[]) =
        if Array.isEmpty updates then
            this
        else
            let mapper (update: TileUpdate) =
                let index = this.GetFlatIndex(update.X, update.Y)
                (index, update.Tile)

            { this with
                Tiles = PersistentVector.updateManyWith mapper updates this.Tiles }

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
