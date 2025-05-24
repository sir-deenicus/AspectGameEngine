namespace AspectGameEngine

open FSharpx.Collections

type EditorTileMap =
    { Width: int
      Height: int
      Tiles: PersistentVector<PersistentVector<Tile>>
      VoidSpriteLoc: SpriteLoc
      MapName: string
      MapType: MapType
      TilePropertiesReference: Map<SpriteLoc, TileProperties> }

    member this.New(width, height, voidSpriteLoc, tilePropertiesReference) =
        { Width = width
          Height = height
          Tiles =
            PersistentVector.init height (fun _ ->
                PersistentVector.init width (fun _ ->
                    { SpriteLoc = voidSpriteLoc
                      Health = 0
                      DestroyedSpriteLoc = None }))
          VoidSpriteLoc = voidSpriteLoc
          MapName = ""
          MapType = MapType.Room
          TilePropertiesReference = tilePropertiesReference }

    member this.GetTile(x, y) = this.Tiles.[y].[x]

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
        { this with
            Tiles = this.Tiles.Update(y, this.Tiles.[y].Update(x, tile)) }

    member this.UpdateWidth width = this.Resize(width, this.Height)
    member this.UpdateHeight height = this.Resize(this.Width, height)

    member this.Resize(width, height) =
        let emptyTile =
            { SpriteLoc = this.VoidSpriteLoc
              Health = 0
              DestroyedSpriteLoc = None }

        // First handle height changes
        let adjustedRows =
            if height > this.Height then
                // Add new rows
                let newRows =
                    PersistentVector.init (height - this.Height) (fun _ ->
                        PersistentVector.init width (fun _ -> emptyTile))

                PersistentVector.append this.Tiles newRows
            elif height < this.Height then
                // Remove rows using take
                PersistentVector.take height this.Tiles
            else
                this.Tiles // Height unchanged

        // Then handle width changes for each row
        let finalTiles =
            PersistentVector.map
                (fun (row: PersistentVector<Tile>) ->
                    let currentActualRowWidth = row.Length // Get current row's length

                    if width > currentActualRowWidth then
                        // Extend row
                        let additionalTiles =
                            PersistentVector.init (width - currentActualRowWidth) (fun _ -> emptyTile)

                        PersistentVector.append row additionalTiles
                    elif width < currentActualRowWidth then
                        // Truncate row using take
                        PersistentVector.take width row
                    else
                        // Width unchanged for this specific row
                        row)
                adjustedRows

        { this with
            Width = width
            Height = height
            Tiles = finalTiles }

type EditorHistory =
    { History: List<EditorTileMap>
      CurrentIndex: int
      MaxHistorySize: int }

    member this.HistorySize = this.History.Length
    member this.CanUndo = this.CurrentIndex < this.History.Length - 1
    member this.CanRedo = this.CurrentIndex > 0

    member this.New(maxHistorySize) =
        { History = []
          CurrentIndex = 0
          MaxHistorySize = maxHistorySize }

    member this.UpdateTile(x: int, y: int, tile: Tile) =
        let current = this.History[this.CurrentIndex].UpdateTile(x, y, tile)
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
