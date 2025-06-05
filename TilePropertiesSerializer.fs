namespace AspectGameEngine

open AspectGameEngine.FBS
open Google.FlatBuffers

module TilePropertiesSerializer =
    // Convert F# enums to FlatBuffers enums
    let private toBiomeFBS (biome: Biome) : BiomeFBS =
        match biome with
        | Biome.None -> BiomeFBS.None
        | Biome.Forest -> BiomeFBS.Forest
        | Biome.Desert -> BiomeFBS.Desert
        | Biome.Snow -> BiomeFBS.Snow
        | Biome.Swamp -> BiomeFBS.Swamp
        | Biome.Mountain -> BiomeFBS.Mountain
        | Biome.Ocean -> BiomeFBS.Ocean
        | Biome.Plains -> BiomeFBS.Plains
        | Biome.AlchemyLab -> BiomeFBS.AlchemyLab
        | Biome.EnchantersLab -> BiomeFBS.EnchantersLab
        | Biome.Blacksmith -> BiomeFBS.Blacksmith
        | _ -> BiomeFBS.None

    let private fromBiomeFBS (biomeFBS: BiomeFBS) : Biome =
        match biomeFBS with
        | BiomeFBS.None -> Biome.None
        | BiomeFBS.Forest -> Biome.Forest
        | BiomeFBS.Desert -> Biome.Desert
        | BiomeFBS.Snow -> Biome.Snow
        | BiomeFBS.Swamp -> Biome.Swamp
        | BiomeFBS.Mountain -> Biome.Mountain
        | BiomeFBS.Ocean -> Biome.Ocean
        | BiomeFBS.Plains -> Biome.Plains
        | BiomeFBS.AlchemyLab -> Biome.AlchemyLab
        | BiomeFBS.EnchantersLab -> Biome.EnchantersLab
        | BiomeFBS.Blacksmith -> Biome.Blacksmith
        | _ -> Biome.None

    let private toTileTypeFBS (tileType: TileType) : TileTypeFBS =
        match tileType with
        | TileType.NullTile -> TileTypeFBS.NullTile
        | TileType.Void -> TileTypeFBS.Void
        | TileType.Wall -> TileTypeFBS.Wall
        | TileType.Floor -> TileTypeFBS.Floor
        | TileType.Door -> TileTypeFBS.Door
        | TileType.Lever -> TileTypeFBS.NullTile // Map to NullTile as Lever doesn't exist in FBS
        | TileType.Stairs -> TileTypeFBS.Stairs
        | TileType.Water -> TileTypeFBS.Water
        | TileType.Lava -> TileTypeFBS.Lava
        | TileType.City -> TileTypeFBS.City
        | _ -> TileTypeFBS.NullTile

    let private fromTileTypeFBS (tileTypeFBS: TileTypeFBS) : TileType =
        match tileTypeFBS with
        | TileTypeFBS.NullTile -> TileType.NullTile
        | TileTypeFBS.Void -> TileType.Void
        | TileTypeFBS.Wall -> TileType.Wall
        | TileTypeFBS.Floor -> TileType.Floor
        | TileTypeFBS.Door -> TileType.Door
        | TileTypeFBS.Stairs -> TileType.Stairs
        | TileTypeFBS.Water -> TileType.Water
        | TileTypeFBS.Lava -> TileType.Lava
        | TileTypeFBS.City -> TileType.City
        | _ -> TileType.NullTile

    let private toTileOpacityFBS (opacity: TileOpacity) : TileOpacityFBS =
        match opacity with
        | TileOpacity.Opaque -> TileOpacityFBS.Opaque
        | TileOpacity.Transparent -> TileOpacityFBS.Transparent
        | _ -> TileOpacityFBS.Opaque

    let private fromTileOpacityFBS (opacityFBS: TileOpacityFBS) : TileOpacity =
        match opacityFBS with
        | TileOpacityFBS.Opaque -> TileOpacity.Opaque
        | TileOpacityFBS.Transparent -> TileOpacity.Transparent
        | _ -> TileOpacity.Opaque

    let private createSpriteLocFBS (builder: Google.FlatBuffers.FlatBufferBuilder) (spriteLoc: SpriteLoc) =
        SpriteLocFBS.CreateSpriteLocFBS(builder, spriteLoc.AtlasIndex, spriteLoc.Row, spriteLoc.Column)

    let private createSpriteLoc (spriteLocFBS: SpriteLocFBS) : SpriteLoc =
        SpriteLoc(spriteLocFBS.AtlasIndex, spriteLocFBS.Row, spriteLocFBS.Column)

    let serialize (tileProps: TilePropertiesImmutableReference) : byte[] =
        // Estimate buffer size based on content
        let entryCount = tileProps.Properties.Count

        let estimatedSize =
            if entryCount = 0 then
                256
            else
                // Rough estimate: 200 bytes per entry + base overhead
                // This accounts for strings, structs, and table overhead
                max 1024 (entryCount * 200 + 512)

        let builder = FlatBufferBuilder(estimatedSize)

        // Create tile set name string
        let tileSetNameOffset = builder.CreateString(tileProps.GetTileSetName())

        let entries =
            [| for KeyValue(spriteLoc, properties) in tileProps.Properties do
                   let descKeyOffset = builder.CreateString(properties.DescriptionKey)
                   //Create destroyed sprite loc (if exists)
                   let destroyedSpriteLocOffset =
                       Option.map (createSpriteLocFBS builder) properties.DestroyedSpriteLoc

                   // Create state changed sprite loc (if exists)
                   let stateChangedSpriteLocOffset =
                       Option.map (createSpriteLocFBS builder) properties.StateChangedSpriteLoc

                   // Start building TilePropertiesFBS (C# generated class)
                   TilePropertiesFBS.StartTilePropertiesFBS(builder)
                   TilePropertiesFBS.AddWalkable(builder, properties.Walkable)
                   TilePropertiesFBS.AddIsVoid(builder, properties.IsVoid)
                   TilePropertiesFBS.AddTileType(builder, toTileTypeFBS properties.TileType)
                   TilePropertiesFBS.AddHealth(builder, properties.Health)
                   TilePropertiesFBS.AddDescriptionKey(builder, descKeyOffset)
                   TilePropertiesFBS.AddBiome(builder, toBiomeFBS properties.Biome)
                   TilePropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS properties.TileOpacity)

                   if destroyedSpriteLocOffset.IsSome then
                       TilePropertiesFBS.AddDestroyedSpriteLoc(builder, destroyedSpriteLocOffset.Value)

                   if stateChangedSpriteLocOffset.IsSome then
                       TilePropertiesFBS.AddStateChangedSpriteLoc(builder, stateChangedSpriteLocOffset.Value)

                   let tilePropsOffset = TilePropertiesFBS.EndTilePropertiesFBS(builder)

                   // Create the key (SpriteLoc)
                   let keyFBS = createSpriteLocFBS builder spriteLoc

                   // Create TilePropertiesEntryFBS (C# generated class)
                   TilePropertiesEntryFBS.StartTilePropertiesEntryFBS(builder)
                   TilePropertiesEntryFBS.AddKey(builder, keyFBS)
                   TilePropertiesEntryFBS.AddValue(builder, tilePropsOffset)
                   yield TilePropertiesEntryFBS.EndTilePropertiesEntryFBS(builder) |]

        // Create vector of entries
        let entriesVector = TilePropertiesSetFBS.CreateEntriesVector(builder, entries)

        // Create root TilePropertiesSetFBS (C# generated class)
        TilePropertiesSetFBS.StartTilePropertiesSetFBS(builder)
        TilePropertiesSetFBS.AddTileSetName(builder, tileSetNameOffset)
        TilePropertiesSetFBS.AddEntries(builder, entriesVector)
        let rootOffset = TilePropertiesSetFBS.EndTilePropertiesSetFBS(builder)

        // Finish and return bytes
        builder.Finish(rootOffset.Value)
        builder.SizedByteArray()

    let deserialize (bytes: byte[]) : TilePropertiesImmutableReference =
        let buffer = ByteBuffer(bytes)
        let tilePropsSet = TilePropertiesSetFBS.GetRootAsTilePropertiesSetFBS(buffer) // .GetRootAs(buffer)

        let tileSetName = tilePropsSet.TileSetName
        let mutable result = TilePropertiesImmutableReference.New(tileSetName)

        for i in 0 .. tilePropsSet.EntriesLength - 1 do
            let entry = tilePropsSet.Entries(i)

            match Option.ofNullable entry with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Key, Option.ofNullable entry.Value with
                | Some keyFBS, Some valueFBS ->
                    let spriteLoc = createSpriteLoc keyFBS

                    let destroyedSpriteLoc =
                        Option.ofNullable valueFBS.DestroyedSpriteLoc |> Option.map createSpriteLoc

                    let stateChangedSpriteLoc =
                        Option.ofNullable valueFBS.StateChangedSpriteLoc |> Option.map createSpriteLoc

                    let tileProperties =
                        { Walkable = valueFBS.Walkable
                          IsVoid = valueFBS.IsVoid
                          TileType = fromTileTypeFBS valueFBS.TileType
                          Health = valueFBS.Health
                          DescriptionKey = valueFBS.DescriptionKey
                          Biome = fromBiomeFBS valueFBS.Biome
                          TileOpacity = fromTileOpacityFBS valueFBS.TileOpacity
                          DestroyedSpriteLoc = destroyedSpriteLoc
                          StateChangedSpriteLoc = stateChangedSpriteLoc }

                    result <- result.Set(spriteLoc, tileProperties)
                | _ -> ()

        result
