namespace AspectGameEngine

open AspectGameEngine.FBS
open Google.FlatBuffers

module TileMapSerializer =
    let private toMapTypeFBS (mapType: MapType) : MapTypeFBS =
        match mapType with
        | MapType.Room -> MapTypeFBS.Room
        | MapType.ComplexBuilding -> MapTypeFBS.ComplexBuilding
        | MapType.TownOrCity -> MapTypeFBS.TownOrCity
        | MapType.Dungeon -> MapTypeFBS.Dungeon
        | MapType.Overworld -> MapTypeFBS.Overworld
        | _ -> MapTypeFBS.Room

    let private fromMapTypeFBS (mapTypeFBS: MapTypeFBS) : MapType =
        match mapTypeFBS with
        | MapTypeFBS.Room -> MapType.Room
        | MapTypeFBS.ComplexBuilding -> MapType.ComplexBuilding
        | MapTypeFBS.TownOrCity -> MapType.TownOrCity
        | MapTypeFBS.Dungeon -> MapType.Dungeon
        | MapTypeFBS.Overworld -> MapType.Overworld
        | _ -> MapType.Room

    let private createSpriteLocFBS (builder: FlatBufferBuilder) (spriteLoc: SpriteLoc) =
        SpriteLocFBS.CreateSpriteLocFBS(builder, spriteLoc.AtlasIndex, spriteLoc.Row, spriteLoc.Column)

    let private createSpriteLoc (spriteLocFBS: SpriteLocFBS) : SpriteLoc =
        SpriteLoc(spriteLocFBS.AtlasIndex, spriteLocFBS.Row, spriteLocFBS.Column)

    let private createTileFBS (builder: FlatBufferBuilder) (tile: Tile) =
        let spriteLocOffset = createSpriteLocFBS builder tile.SpriteLoc
        TileFBS.CreateTileFBS(builder, spriteLocOffset, tile.Health, tile.IsOccupied)

    let private createLayerCellFBS (builder: FlatBufferBuilder) (cell: LayerCell) =
        let itemsArray = cell.Items.ToArray()
        let itemsVector = LayerCellFBS.CreateItemsVector(builder, itemsArray)
        let fixtureId = match cell.FixtureId with Some id -> id | None -> -1
        let actorId = match cell.ActorId with Some id -> id | None -> -1
        let decalId = match LayerQueries.TryGetTopDecalId cell with Some id -> id | None -> -1

        let decalsView = LayerQueries.GetDecalView cell
        let decalsVector =
            if decalsView.Count = 0 then
                Unchecked.defaultof<VectorOffset>
            else
                let decalsArray = Array.init decalsView.Count (fun i -> decalsView.Decals.[i])
                LayerCellFBS.CreateDecalsVector(builder, decalsArray)

        LayerCellFBS.StartLayerCellFBS(builder)
        LayerCellFBS.AddItems(builder, itemsVector)
        LayerCellFBS.AddFixtureId(builder, fixtureId)
        LayerCellFBS.AddActorId(builder, actorId)
        LayerCellFBS.AddDecalId(builder, decalId)
        if decalsView.Count > 0 then
            LayerCellFBS.AddDecals(builder, decalsVector)
        LayerCellFBS.EndLayerCellFBS(builder)

    let serialize (tileMap: TileMap) : byte[] =
        // Estimate buffer size
        let tileCount = tileMap.Width * tileMap.Height
        let estimatedSize = max 4096 (tileCount * 32 + 2048)
        let builder = FlatBufferBuilder(estimatedSize)

        // Create tiles vector
        let tilesOffsets = [| for tile in tileMap.Tiles -> createTileFBS builder tile |]
        let tilesVector = TileMapFBS.CreateTilesVector(builder, tilesOffsets)

        // Create layer cells vector
        let layerCellsOffsets = [| for cell in tileMap.LayerCells -> createLayerCellFBS builder cell |]
        let layerCellsVector = TileMapFBS.CreateLayerCellsVector(builder, layerCellsOffsets)

        let voidSpriteLocOffset = createSpriteLocFBS builder tileMap.VoidSpriteLoc
        let mapNameOffset = builder.CreateString(tileMap.MapName)
        let tilesetNameOffset = builder.CreateString(tileMap.TileSetName)

        TileMapFBS.StartTileMapFBS(builder)
        TileMapFBS.AddWidth(builder, tileMap.Width)
        TileMapFBS.AddHeight(builder, tileMap.Height)
        TileMapFBS.AddTiles(builder, tilesVector)
        TileMapFBS.AddLayerCells(builder, layerCellsVector)
        TileMapFBS.AddVoidSpriteLoc(builder, voidSpriteLocOffset)
        TileMapFBS.AddMapName(builder, mapNameOffset)
        TileMapFBS.AddMapType(builder, toMapTypeFBS tileMap.MapType)
        TileMapFBS.AddTilesetName(builder, tilesetNameOffset)
        let rootOffset = TileMapFBS.EndTileMapFBS(builder)

        builder.Finish(rootOffset.Value)
        builder.SizedByteArray()

    let deserialize (bytes: byte[]) : TileMap =
        let buffer = ByteBuffer(bytes)
        let tileMapFBS = TileMapFBS.GetRootAsTileMapFBS(buffer)

        // Deserialize tiles
        let tiles =
            [| for i in 0 .. tileMapFBS.TilesLength - 1 do
                   match Option.ofNullable (tileMapFBS.Tiles(i)) with
                   | Some tileFBS ->
                       let spriteLoc = createSpriteLoc tileFBS.SpriteLoc.Value
                       yield { SpriteLoc = spriteLoc; Health = tileFBS.Health; IsOccupied = tileFBS.IsOccupied }
                   | None -> yield { SpriteLoc = SpriteLoc(0, 0, 0); Health = 0; IsOccupied = false } |]

        // Deserialize layer cells
        let layerCells =
            [| for i in 0 .. tileMapFBS.LayerCellsLength - 1 do
                   match Option.ofNullable (tileMapFBS.LayerCells(i)) with
                   | Some cellFBS ->
                       let items = ResizeArray(cellFBS.GetItemsArray())
                       let fixtureId = if cellFBS.FixtureId = -1 then None else Some cellFBS.FixtureId
                       let actorId = if cellFBS.ActorId = -1 then None else Some cellFBS.ActorId
                       let cell =
                           { LayerCell.Items = items
                             FixtureId = fixtureId
                             ActorId = actorId
                             Decals = null
                             DecalCount = 0 }

                       // Prefer decals vector; fall back to legacy single decal_id.
                       if cellFBS.DecalsLength > 0 then
                           for j = 0 to cellFBS.DecalsLength - 1 do
                               LayerQueries.AddDecal(cell, cellFBS.Decals(j))
                       else
                           if cellFBS.DecalId <> -1 then
                               LayerQueries.AddDecal(cell, cellFBS.DecalId)

                       yield cell
                   | None -> yield LayerCell.Create() |]

        let voidSpriteLoc = createSpriteLoc tileMapFBS.VoidSpriteLoc.Value
        let tilesetName = tileMapFBS.TilesetName

        TileMap(
            tileMapFBS.Width,
            tileMapFBS.Height,
            tiles,
            layerCells,
            voidSpriteLoc,
            tilesetName,
            tileMapFBS.MapName,
            fromMapTypeFBS tileMapFBS.MapType
        )