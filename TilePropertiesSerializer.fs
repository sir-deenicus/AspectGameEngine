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
        | _ -> BiomeFBS.None

    let internal fromBiomeFBS (biomeFBS: BiomeFBS) : Biome =
        match biomeFBS with
        | BiomeFBS.None -> Biome.None
        | BiomeFBS.Forest -> Biome.Forest
        | BiomeFBS.Desert -> Biome.Desert
        | BiomeFBS.Snow -> Biome.Snow
        | BiomeFBS.Swamp -> Biome.Swamp
        | BiomeFBS.Mountain -> Biome.Mountain
        | BiomeFBS.Ocean -> Biome.Ocean
        | BiomeFBS.Plains -> Biome.Plains 
        | _ -> Biome.None

    let private toTileTypeFBS (tileType: TileType) : TileTypeFBS =
        match tileType with
        | TileType.NullTile -> TileTypeFBS.NullTile
        | TileType.Void -> TileTypeFBS.Void
        | TileType.Wall -> TileTypeFBS.Wall
        | TileType.Floor -> TileTypeFBS.Floor
        | TileType.Ground -> TileTypeFBS.Ground
        | TileType.Door -> TileTypeFBS.Door
        | TileType.Lever -> TileTypeFBS.Lever
        | TileType.Stairs -> TileTypeFBS.Stairs
        | TileType.Water -> TileTypeFBS.Water
        | TileType.Lava -> TileTypeFBS.Lava
        | TileType.CityOrTown -> TileTypeFBS.CityOrTown
        | TileType.Fixture -> TileTypeFBS.Fixture
        | _ -> TileTypeFBS.NullTile

    let internal fromTileTypeFBS (tileTypeFBS: TileTypeFBS) : TileType =
        match tileTypeFBS with
        | TileTypeFBS.NullTile -> TileType.NullTile
        | TileTypeFBS.Void -> TileType.Void
        | TileTypeFBS.Wall -> TileType.Wall
        | TileTypeFBS.Floor -> TileType.Floor
        | TileTypeFBS.Ground -> TileType.Ground
        | TileTypeFBS.Door -> TileType.Door
        | TileTypeFBS.Lever -> TileType.Lever
        | TileTypeFBS.Stairs -> TileType.Stairs
        | TileTypeFBS.Water -> TileType.Water
        | TileTypeFBS.Lava -> TileType.Lava
        | TileTypeFBS.CityOrTown -> TileType.CityOrTown
        | TileTypeFBS.Fixture -> TileType.Fixture
        | _ -> TileType.NullTile

    let private toTileOpacityFBS (opacity: TileOpacity) : TileOpacityFBS =
        match opacity with
        | TileOpacity.Opaque -> TileOpacityFBS.Opaque
        | TileOpacity.Transparent -> TileOpacityFBS.Transparent
        | TileOpacity.Air -> TileOpacityFBS.Air
        | _ -> TileOpacityFBS.Opaque

    let internal fromTileOpacityFBS (opacityFBS: TileOpacityFBS) : TileOpacity =
        match opacityFBS with
        | TileOpacityFBS.Opaque -> TileOpacity.Opaque
        | TileOpacityFBS.Transparent -> TileOpacity.Transparent
        | TileOpacityFBS.Air -> TileOpacity.Air
        | _ -> TileOpacity.Opaque

    let private createSpriteLocFBS (builder: FlatBufferBuilder) (spriteLoc: SpriteLoc) =
        SpriteLocFBS.CreateSpriteLocFBS(builder, spriteLoc.AtlasIndex, spriteLoc.Row, spriteLoc.Column)

    let private createSpriteLoc (spriteLocFBS: SpriteLocFBS) : SpriteLoc =
        SpriteLoc(spriteLocFBS.AtlasIndex, spriteLocFBS.Row, spriteLocFBS.Column)

    let serialize (tileProps: TilePropertiesReference) : byte[] =
        let entryCount = tileProps.GetAllProperties() |> Seq.length
        let estimatedSize = if entryCount = 0 then 256 else max 1024 (entryCount * 200 + 512)
        let builder = FlatBufferBuilder(estimatedSize)

        let tileSetNameOffset = builder.CreateString(tileProps.TileSetName)

        let entries =
            [| for KeyValue(spriteLoc, properties) in tileProps.GetAllProperties() do
                   let descKeyOffset = builder.CreateString(properties.DescriptionKey)
                   let destroyedSpriteLocOffset = Option.map (createSpriteLocFBS builder) properties.DestroyedSpriteLoc
                   let nextStateSpriteLocOffset = Option.map (createSpriteLocFBS builder) properties.NextStateSpriteLoc

                   // Serialize ComplexState union if present
                   let complexStateType, complexStateOffset =
                       match properties.ComplexState with
                       | Some (ComplexState.ClosedDoor state) ->
                           let offset = ClosedDoorStateFBS.CreateClosedDoorStateFBS(builder, state.Locked)
                           (ComplexStateFBS.ClosedDoorStateFBS, offset.Value)
                       | None -> (ComplexStateFBS.NONE, 0)

                   TilePropertiesFBS.StartTilePropertiesFBS(builder)
                   TilePropertiesFBS.AddWalkable(builder, properties.Walkable)
                   TilePropertiesFBS.AddInteractable(builder, properties.Interactable)
                   TilePropertiesFBS.AddTileType(builder, toTileTypeFBS properties.TileType)
                   TilePropertiesFBS.AddHealth(builder, properties.Health)
                   TilePropertiesFBS.AddDescriptionKey(builder, descKeyOffset)
                   TilePropertiesFBS.AddBiome(builder, toBiomeFBS properties.Biome)
                   TilePropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS properties.TileOpacity)

                   if destroyedSpriteLocOffset.IsSome then
                       TilePropertiesFBS.AddDestroyedSpriteLoc(builder, destroyedSpriteLocOffset.Value)
                   if nextStateSpriteLocOffset.IsSome then
                       TilePropertiesFBS.AddNextStateSpriteLoc(builder, nextStateSpriteLocOffset.Value)

                   if complexStateType <> ComplexStateFBS.NONE then
                       TilePropertiesFBS.AddComplexStateType(builder, complexStateType)
                       TilePropertiesFBS.AddComplexState(builder, complexStateOffset)

                   let tilePropsOffset = TilePropertiesFBS.EndTilePropertiesFBS(builder)
                   let keyFBS = createSpriteLocFBS builder spriteLoc

                   TilePropertiesEntryFBS.StartTilePropertiesEntryFBS(builder)
                   TilePropertiesEntryFBS.AddKey(builder, keyFBS)
                   TilePropertiesEntryFBS.AddValue(builder, tilePropsOffset)
                   yield TilePropertiesEntryFBS.EndTilePropertiesEntryFBS(builder) |]

        let entriesVector = TilePropertiesSetFBS.CreateEntriesVector(builder, entries)

        TilePropertiesSetFBS.StartTilePropertiesSetFBS(builder)
        TilePropertiesSetFBS.AddTileSetName(builder, tileSetNameOffset)
        TilePropertiesSetFBS.AddEntries(builder, entriesVector)
        let rootOffset = TilePropertiesSetFBS.EndTilePropertiesSetFBS(builder)

        builder.Finish(rootOffset.Value)
        builder.SizedByteArray()

    let deserialize (bytes: byte[]) : TilePropertiesReference =
        let buffer = ByteBuffer(bytes)
        let tilePropsSet = TilePropertiesSetFBS.GetRootAsTilePropertiesSetFBS(buffer)
        let result = TilePropertiesReference(tilePropsSet.TileSetName)

        for i in 0 .. tilePropsSet.EntriesLength - 1 do
            match Option.ofNullable (tilePropsSet.Entries(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Key, Option.ofNullable entry.Value with
                | Some keyFBS, Some valueFBS ->
                    let spriteLoc = createSpriteLoc keyFBS
                    let destroyedSpriteLoc = Option.ofNullable valueFBS.DestroyedSpriteLoc |> Option.map createSpriteLoc
                    let nextStateSpriteLoc = Option.ofNullable valueFBS.NextStateSpriteLoc |> Option.map createSpriteLoc

                    // Deserialize ComplexState union
                    let complexState =
                        match valueFBS.ComplexStateType with
                        | ComplexStateFBS.ClosedDoorStateFBS ->
                            match Option.ofNullable (valueFBS.ComplexState<ClosedDoorStateFBS>()) with
                            | Some state -> Some (ComplexState.ClosedDoor { Locked = state.Locked })
                            | None -> None
                        | _ -> None

                    let tileProperties =
                        { Walkable = valueFBS.Walkable
                          Interactable = valueFBS.Interactable
                          TileType = fromTileTypeFBS valueFBS.TileType
                          Health = valueFBS.Health
                          DescriptionKey = valueFBS.DescriptionKey
                          Biome = fromBiomeFBS valueFBS.Biome
                          TileOpacity = fromTileOpacityFBS valueFBS.TileOpacity
                          DestroyedSpriteLoc = destroyedSpriteLoc
                          NextStateSpriteLoc = nextStateSpriteLoc
                          ComplexState = complexState }

                    result.[spriteLoc] <- tileProperties
                | _ -> ()
        result
