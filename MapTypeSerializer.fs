namespace AspectGameEngine

open AspectGameEngine.FBS
open Google.FlatBuffers
open System.Collections.Generic

module TileMapSerializer =
    // Convert F# MapType to FlatBuffers MapTypeFBS
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

    let serialize (tileMap: TileMap) : byte[] =
        // Estimate buffer size
        let tileCount = tileMap.Width * tileMap.Height
        let estimatedSize = max 4096 (tileCount * 32 + 2048)
        
        let builder = FlatBufferBuilder(estimatedSize)

        // Serialize the TilePropertiesSet
        let tilePropsSetBytes = TilePropertiesSerializer.serialize (TilePropertiesImmutableReference.FromReference(tileMap.TilePropertiesReference))
        let tilePropsSetBuffer = ByteBuffer(tilePropsSetBytes)
        let tilePropsSet = TilePropertiesSetFBS.GetRootAsTilePropertiesSetFBS(tilePropsSetBuffer)
        
        // We need to copy the TilePropertiesSet into our new builder
        // Create strings and offsets for tile properties
        let tileSetName = builder.CreateString(tilePropsSet.TileSetName)
        
        let tilePropsEntries =
            [| for i in 0 .. tilePropsSet.EntriesLength - 1 do
                   match Option.ofNullable (tilePropsSet.Entries(i)) with
                   | Some entry ->
                       match Option.ofNullable entry.Key, Option.ofNullable entry.Value with
                       | Some keyFBS, Some valueFBS ->
                           let descKeyOffset = builder.CreateString(valueFBS.DescriptionKey)
                           
                           let destroyedSpriteLocOffset =
                               Option.ofNullable valueFBS.DestroyedSpriteLoc
                               |> Option.map (fun loc -> createSpriteLocFBS builder (createSpriteLoc loc))
                           
                           let nextStateSpriteLocOffset =
                               Option.ofNullable valueFBS.NextStateSpriteLoc
                               |> Option.map (fun loc -> createSpriteLocFBS builder (createSpriteLoc loc))
                           
                           TilePropertiesFBS.StartTilePropertiesFBS(builder)
                           TilePropertiesFBS.AddWalkable(builder, valueFBS.Walkable)
                           TilePropertiesFBS.AddTileType(builder, valueFBS.TileType)
                           TilePropertiesFBS.AddHealth(builder, valueFBS.Health)
                           TilePropertiesFBS.AddDescriptionKey(builder, descKeyOffset)
                           TilePropertiesFBS.AddBiome(builder, valueFBS.Biome)
                           TilePropertiesFBS.AddTileOpacity(builder, valueFBS.TileOpacity)
                           
                           if destroyedSpriteLocOffset.IsSome then
                               TilePropertiesFBS.AddDestroyedSpriteLoc(builder, destroyedSpriteLocOffset.Value)
                           
                           if nextStateSpriteLocOffset.IsSome then
                               TilePropertiesFBS.AddNextStateSpriteLoc(builder, nextStateSpriteLocOffset.Value)
                           
                           let tilePropsOffset = TilePropertiesFBS.EndTilePropertiesFBS(builder)
                           
                           let key = createSpriteLoc keyFBS
                           let keyOffset = createSpriteLocFBS builder key
                           
                           TilePropertiesEntryFBS.StartTilePropertiesEntryFBS(builder)
                           TilePropertiesEntryFBS.AddKey(builder, keyOffset)
                           TilePropertiesEntryFBS.AddValue(builder, tilePropsOffset)
                           yield TilePropertiesEntryFBS.EndTilePropertiesEntryFBS(builder)
                       | _ -> ()
                   | None -> ()
            |]
        
        let tilePropsEntriesVector = TilePropertiesSetFBS.CreateEntriesVector(builder, tilePropsEntries)
        
        TilePropertiesSetFBS.StartTilePropertiesSetFBS(builder)
        TilePropertiesSetFBS.AddTileSetName(builder, tileSetName)
        TilePropertiesSetFBS.AddEntries(builder, tilePropsEntriesVector)
        let tilePropsSetOffset = TilePropertiesSetFBS.EndTilePropertiesSetFBS(builder)

        // Create tiles vector
        let tilesOffsets =
            [| for tile in tileMap.Tiles do
                   yield createTileFBS builder tile |]
        
        let tilesVector = TileMapFBS.CreateTilesVector(builder, tilesOffsets)

        // Create void sprite loc
        let voidSpriteLocOffset = createSpriteLocFBS builder tileMap.VoidSpriteLoc

        // Create map name string
        let mapNameOffset = builder.CreateString(tileMap.MapName)

        // Create TileMapFBS
        TileMapFBS.StartTileMapFBS(builder)
        TileMapFBS.AddWidth(builder, tileMap.Width)
        TileMapFBS.AddHeight(builder, tileMap.Height)
        TileMapFBS.AddTiles(builder, tilesVector)
        TileMapFBS.AddVoidSpriteLoc(builder, voidSpriteLocOffset)
        TileMapFBS.AddMapName(builder, mapNameOffset)
        TileMapFBS.AddMapType(builder, toMapTypeFBS tileMap.MapType)
        TileMapFBS.AddTilePropertiesSet(builder, tilePropsSetOffset)
        let rootOffset = TileMapFBS.EndTileMapFBS(builder)

        // Finish and return bytes
        builder.Finish(rootOffset.Value)
        builder.SizedByteArray()

    let deserialize (bytes: byte[]) : TileMap =
        let buffer = ByteBuffer(bytes)
        let tileMapFBS = TileMapFBS.GetRootAsTileMapFBS(buffer)

        // Deserialize TilePropertiesSet
        let tilePropsSet = tileMapFBS.TilePropertiesSet.Value
        let tileSetName = tilePropsSet.TileSetName
        let mutable tilePropsImmutable = TilePropertiesImmutableReference.New(tileSetName)

        for i in 0 .. tilePropsSet.EntriesLength - 1 do
            match Option.ofNullable (tilePropsSet.Entries(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Key, Option.ofNullable entry.Value with
                | Some keyFBS, Some valueFBS ->
                    let spriteLoc = createSpriteLoc keyFBS

                    let destroyedSpriteLoc =
                        Option.ofNullable valueFBS.DestroyedSpriteLoc |> Option.map createSpriteLoc

                    let nextStateSpriteLoc =
                        Option.ofNullable valueFBS.NextStateSpriteLoc |> Option.map createSpriteLoc

                    let tileProperties =
                        { Walkable = valueFBS.Walkable
                          TileType = TilePropertiesSerializer.fromTileTypeFBS valueFBS.TileType
                          Health = valueFBS.Health
                          DescriptionKey = valueFBS.DescriptionKey
                          Biome = TilePropertiesSerializer.fromBiomeFBS valueFBS.Biome
                          TileOpacity = TilePropertiesSerializer.fromTileOpacityFBS valueFBS.TileOpacity
                          DestroyedSpriteLoc = destroyedSpriteLoc
                          NextStateSpriteLoc = nextStateSpriteLoc }

                    tilePropsImmutable <- tilePropsImmutable.Set(spriteLoc, tileProperties)
                | _ -> ()

        let tilePropsReference = TilePropertiesReference()
        for KeyValue(spriteLoc, props) in tilePropsImmutable.Properties do
            tilePropsReference.[spriteLoc] <- props

        // Deserialize tiles
        let tiles =
            [| for i in 0 .. tileMapFBS.TilesLength - 1 do
                   match Option.ofNullable (tileMapFBS.Tiles(i)) with
                   | Some tileFBS ->
                       let spriteLoc = createSpriteLoc tileFBS.SpriteLoc.Value
                       yield { SpriteLoc = spriteLoc
                               Health = tileFBS.Health
                               IsOccupied = tileFBS.IsOccupied }
                   | None ->
                       yield { SpriteLoc = SpriteLoc(0, 0, 0)
                               Health = 0
                               IsOccupied = false }
            |]

        let voidSpriteLoc = createSpriteLoc tileMapFBS.VoidSpriteLoc.Value

        TileMap(
            tileMapFBS.Width,
            tileMapFBS.Height,
            tiles,
            voidSpriteLoc,
            tilePropsReference,
            tileMapFBS.MapName,
            fromMapTypeFBS tileMapFBS.MapType
        )