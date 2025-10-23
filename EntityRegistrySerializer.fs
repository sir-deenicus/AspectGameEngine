namespace AspectGameEngine

open System
open System.Collections.Generic
open Google.FlatBuffers
open AspectGameEngine.FBS

module EntityRegistrySerializer =

    //=== Enum mappers ===

    let private toTileOpacityFBS (opacity: TileOpacity) : TileOpacityFBS =
        match opacity with
        | TileOpacity.Opaque -> TileOpacityFBS.Opaque
        | TileOpacity.Transparent -> TileOpacityFBS.Transparent
        | TileOpacity.Air -> TileOpacityFBS.Air
        | _ -> TileOpacityFBS.Opaque

    let private fromTileOpacityFBS (opacityFBS: TileOpacityFBS) : TileOpacity =
        match opacityFBS with
        | TileOpacityFBS.Opaque -> TileOpacity.Opaque
        | TileOpacityFBS.Transparent -> TileOpacity.Transparent
        | TileOpacityFBS.Air -> TileOpacity.Air
        | _ -> TileOpacity.Opaque

    //=== SpriteRef helpers ===

    let private buildSpriteSheetCell (builder: FlatBufferBuilder) (c: SpriteSheetCell) =
        SpriteSheetCellFBS.CreateSpriteSheetCellFBS(builder, c.SheetId, c.Row, c.Column)

    let private buildSpriteSheetRegion (builder: FlatBufferBuilder) (r: SpriteSheetRegion) =
        SpriteSheetRegionFBS.CreateSpriteSheetRegionFBS(builder, r.SheetId, r.X, r.Y, r.Width, r.Height)

    // Returns: (unionType, unionValueOffset)
    let private buildSpriteRef (builder: FlatBufferBuilder) (sprite: SpriteRef) : SpriteRefFBS * int =
        match sprite with
        | SpriteRef.SheetRegion r ->
            let o = buildSpriteSheetRegion builder r
            (SpriteRefFBS.SpriteSheetRegionFBS, o.Value)
        | SpriteRef.SheetCell c ->
            let o = buildSpriteSheetCell builder c
            (SpriteRefFBS.SpriteSheetCellFBS, o.Value)
        | SpriteRef.TextureId id ->
            let o = TextureIdFBS.CreateTextureIdFBS(builder, id)
            (SpriteRefFBS.TextureIdFBS, o.Value)
        | SpriteRef.Scene path ->
            let s = builder.CreateString(path)
            let o = SceneRefFBS.CreateSceneRefFBS(builder, s)
            (SpriteRefFBS.SceneRefFBS, o.Value)

    let private tryReadSpriteRefFromItem (ip: ItemPropertiesFBS) : SpriteRef option =
        match ip.SpriteType with
        | SpriteRefFBS.SpriteSheetRegionFBS ->
            match Option.ofNullable (ip.Sprite<SpriteSheetRegionFBS>()) with
            | Some s -> Some (SpriteRef.SheetRegion { SheetId = s.SheetId; X = s.X; Y = s.Y; Width = s.Width; Height = s.Height })
            | None -> None
        | SpriteRefFBS.SpriteSheetCellFBS ->
            match Option.ofNullable (ip.Sprite<SpriteSheetCellFBS>()) with
            | Some s -> Some (SpriteRef.SheetCell (SpriteSheetCell(sheetId = s.SheetId, row = s.Row, column = s.Column )))
            | None -> None
        | SpriteRefFBS.TextureIdFBS ->
            match Option.ofNullable (ip.Sprite<TextureIdFBS>()) with
            | Some s -> Some (SpriteRef.TextureId s.Id)
            | None -> None
        | SpriteRefFBS.SceneRefFBS ->
            match Option.ofNullable (ip.Sprite<SceneRefFBS>()) with
            | Some s -> Some (SpriteRef.Scene s.Path)
            | None -> None
        | _ -> None

    let private tryReadSpriteRefFromFixture (fp: FixturePropertiesFBS) : SpriteRef option =
        match fp.SpriteType with
        | SpriteRefFBS.SpriteSheetRegionFBS ->
            match Option.ofNullable (fp.Sprite<SpriteSheetRegionFBS>()) with
            | Some s -> Some (SpriteRef.SheetRegion { SheetId = s.SheetId; X = s.X; Y = s.Y; Width = s.Width; Height = s.Height })
            | None -> None
        | SpriteRefFBS.SpriteSheetCellFBS ->
            match Option.ofNullable (fp.Sprite<SpriteSheetCellFBS>()) with
            | Some s -> Some (SpriteRef.SheetCell(SpriteSheetCell(s.SheetId, s.Row, s.Column)))
            | None -> None
        | SpriteRefFBS.TextureIdFBS ->
            match Option.ofNullable (fp.Sprite<TextureIdFBS>()) with
            | Some s -> Some (SpriteRef.TextureId s.Id)
            | None -> None
        | SpriteRefFBS.SceneRefFBS ->
            match Option.ofNullable (fp.Sprite<SceneRefFBS>()) with
            | Some s -> Some (SpriteRef.Scene s.Path)
            | None -> None
        | _ -> None

    let private tryReadSpriteRefFromActor (ap: ActorPropertiesFBS) : SpriteRef option =
        match ap.SpriteType with
        | SpriteRefFBS.SpriteSheetRegionFBS ->
            match Option.ofNullable (ap.Sprite<SpriteSheetRegionFBS>()) with
            | Some s -> Some (SpriteRef.SheetRegion { SheetId = s.SheetId; X = s.X; Y = s.Y; Width = s.Width; Height = s.Height })
            | None -> None
        | SpriteRefFBS.SpriteSheetCellFBS ->
            match Option.ofNullable (ap.Sprite<SpriteSheetCellFBS>()) with
            | Some s -> Some (SpriteRef.SheetCell(SpriteSheetCell(sheetId = s.SheetId, row = s.Row, column = s.Column)))
            | None -> None
        | SpriteRefFBS.TextureIdFBS ->
            match Option.ofNullable (ap.Sprite<TextureIdFBS>()) with
            | Some s -> Some (SpriteRef.TextureId s.Id)
            | None -> None
        | SpriteRefFBS.SceneRefFBS ->
            match Option.ofNullable (ap.Sprite<SceneRefFBS>()) with
            | Some s -> Some (SpriteRef.Scene s.Path)
            | None -> None
        | _ -> None

    let private tryReadSpriteRefFromDecal (de: DecalPropsEntryFBS) : SpriteRef option =
        match de.SpriteType with
        | SpriteRefFBS.SpriteSheetRegionFBS ->
            match Option.ofNullable (de.Sprite<SpriteSheetRegionFBS>()) with
            | Some s -> Some (SpriteRef.SheetRegion { SheetId = s.SheetId; X = s.X; Y = s.Y; Width = s.Width; Height = s.Height })
            | None -> None
        | SpriteRefFBS.SpriteSheetCellFBS ->
            match Option.ofNullable (de.Sprite<SpriteSheetCellFBS>()) with 
            | Some s -> Some (SpriteRef.SheetCell(SpriteSheetCell(sheetId = s.SheetId, row = s.Row, column = s.Column )))
            | None -> None
        | SpriteRefFBS.TextureIdFBS ->
            match Option.ofNullable (de.Sprite<TextureIdFBS>()) with
            | Some s -> Some (SpriteRef.TextureId s.Id)
            | None -> None
        | SpriteRefFBS.SceneRefFBS ->
            match Option.ofNullable (de.Sprite<SceneRefFBS>()) with
            | Some s -> Some (SpriteRef.Scene s.Path)
            | None -> None
        | _ -> None

    //=== Public data shape for deserialization ===

    type RegistryData =
        { Items: (int * ItemProperties) array
          Fixtures: (int * FixtureProperties) array
          Actors: (int * ActorProperties) array
          Decals: (int * SpriteRef) array }

    //=== Serialization ===

    let serializeFrom
        (items: IDictionary<int, ItemProperties>)
        (fixtures: IDictionary<int, FixtureProperties>)
        (actors: IDictionary<int, ActorProperties>)
        (decals: IDictionary<int, SpriteRef>) : byte[] =

        let countEstimate =
            items.Count * 32 + fixtures.Count * 40 + actors.Count * 36 + decals.Count * 24 + 1024
        let builder = FlatBufferBuilder(max 2048 countEstimate)

        // Items
        let itemOffsets =
            [| for KeyValue(id, props) in items ->
                let sType, sOff = buildSpriteRef builder props.Sprite
                ItemPropertiesFBS.StartItemPropertiesFBS(builder)
                ItemPropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS props.TileOpacity)
                ItemPropertiesFBS.AddSpriteType(builder, sType)
                ItemPropertiesFBS.AddSprite(builder, sOff)
                let propsOff = ItemPropertiesFBS.EndItemPropertiesFBS(builder)

                ItemPropsEntryFBS.StartItemPropsEntryFBS(builder)
                ItemPropsEntryFBS.AddId(builder, id)
                ItemPropsEntryFBS.AddProps(builder, propsOff)
                ItemPropsEntryFBS.EndItemPropsEntryFBS(builder) |]

        let itemsVec =
            if itemOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateItemsVector(builder, itemOffsets))

        // Fixtures
        let fixtureOffsets =
            [| for KeyValue(id, props) in fixtures ->
                let sType, sOff = buildSpriteRef builder props.Sprite
                FixturePropertiesFBS.StartFixturePropertiesFBS(builder)
                FixturePropertiesFBS.AddBlocksMovement(builder, props.BlocksMovement)
                FixturePropertiesFBS.AddInteractable(builder, props.Interactable)
                FixturePropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS props.TileOpacity)
                FixturePropertiesFBS.AddSpriteType(builder, sType)
                FixturePropertiesFBS.AddSprite(builder, sOff)
                let propsOff = FixturePropertiesFBS.EndFixturePropertiesFBS(builder)

                FixturePropsEntryFBS.StartFixturePropsEntryFBS(builder)
                FixturePropsEntryFBS.AddId(builder, id)
                FixturePropsEntryFBS.AddProps(builder, propsOff)
                FixturePropsEntryFBS.EndFixturePropsEntryFBS(builder) |]

        let fixturesVec =
            if fixtureOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateFixturesVector(builder, fixtureOffsets))

        // Actors
        let actorOffsets =
            [| for KeyValue(id, props) in actors ->
                let sType, sOff = buildSpriteRef builder props.Sprite
                ActorPropertiesFBS.StartActorPropertiesFBS(builder)
                ActorPropertiesFBS.AddBlocksMovement(builder, props.BlocksMovement)
                ActorPropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS props.TileOpacity)
                ActorPropertiesFBS.AddSpriteType(builder, sType)
                ActorPropertiesFBS.AddSprite(builder, sOff)
                let propsOff = ActorPropertiesFBS.EndActorPropertiesFBS(builder)

                ActorPropsEntryFBS.StartActorPropsEntryFBS(builder)
                ActorPropsEntryFBS.AddId(builder, id)
                ActorPropsEntryFBS.AddProps(builder, propsOff)
                ActorPropsEntryFBS.EndActorPropsEntryFBS(builder) |]

        let actorsVec =
            if actorOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateActorsVector(builder, actorOffsets))

        // Decals
        let decalOffsets =
            [| for KeyValue(id, sprite) in decals ->
                let sType, sOff = buildSpriteRef builder sprite
                DecalPropsEntryFBS.StartDecalPropsEntryFBS(builder)
                DecalPropsEntryFBS.AddId(builder, id)
                DecalPropsEntryFBS.AddSpriteType(builder, sType)
                DecalPropsEntryFBS.AddSprite(builder, sOff)
                DecalPropsEntryFBS.EndDecalPropsEntryFBS(builder) |]

        let decalsVec =
            if decalOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateDecalsVector(builder, decalOffsets))

        EntityRegistryFBS.StartEntityRegistryFBS(builder)
        if itemsVec.HasValue then EntityRegistryFBS.AddItems(builder, itemsVec.Value)
        if fixturesVec.HasValue then EntityRegistryFBS.AddFixtures(builder, fixturesVec.Value)
        if actorsVec.HasValue then EntityRegistryFBS.AddActors(builder, actorsVec.Value)
        if decalsVec.HasValue then EntityRegistryFBS.AddDecals(builder, decalsVec.Value)
        let root = EntityRegistryFBS.EndEntityRegistryFBS(builder)

        builder.Finish(root.Value)
        builder.SizedByteArray()

    // Convenience: serialize current global registry
    let serializeCurrent() =
        serializeFrom
            (EntityRegistry.ItemProps :> IDictionary<_, _>)
            (EntityRegistry.FixtureProps :> IDictionary<_, _>)
            (EntityRegistry.ActorProps :> IDictionary<_, _>)
            (EntityRegistry.DecalProps :> IDictionary<_, _>)

    //=== Deserialization ===

    let deserialize (bytes: byte[]) : RegistryData =
        let bb = ByteBuffer(bytes)
        let root = EntityRegistryFBS.GetRootAsEntityRegistryFBS(bb)

        // Items
        let items =
            [| for i in 0 .. root.ItemsLength - 1 do
                   match Option.ofNullable (root.Items(i)) with
                   | None -> ()
                   | Some entry ->
                       match Option.ofNullable entry.Props with
                       | None -> ()
                       | Some ip ->
                           match tryReadSpriteRefFromItem ip with
                           | None -> ()
                           | Some sprite ->
                               let props =
                                   { ItemProperties.Sprite = sprite
                                     TileOpacity = fromTileOpacityFBS ip.TileOpacity }
                               yield (entry.Id, props) |]

        // Fixtures
        let fixtures =
            [| for i in 0 .. root.FixturesLength - 1 do
                   match Option.ofNullable (root.Fixtures(i)) with
                   | None -> ()
                   | Some entry ->
                       match Option.ofNullable entry.Props with
                       | None -> ()
                       | Some fp ->
                           match tryReadSpriteRefFromFixture fp with
                           | None -> ()
                           | Some sprite ->
                               let props =
                                   { FixtureProperties.Sprite = sprite
                                     BlocksMovement = fp.BlocksMovement
                                     Interactable = fp.Interactable
                                     TileOpacity = fromTileOpacityFBS fp.TileOpacity }
                               yield (entry.Id, props) |]

        // Actors
        let actors =
            [| for i in 0 .. root.ActorsLength - 1 do
                   match Option.ofNullable (root.Actors(i)) with
                   | None -> ()
                   | Some entry ->
                       match Option.ofNullable entry.Props with
                       | None -> ()
                       | Some ap ->
                           match tryReadSpriteRefFromActor ap with
                           | None -> ()
                           | Some sprite ->
                               let props =
                                   { ActorProperties.Sprite = sprite
                                     BlocksMovement = ap.BlocksMovement
                                     TileOpacity = fromTileOpacityFBS ap.TileOpacity }
                               yield (entry.Id, props) |]

        // Decals
        let decals =
            [| for i in 0 .. root.DecalsLength - 1 do
                   match Option.ofNullable (root.Decals(i)) with
                   | None -> ()
                   | Some entry ->
                       match tryReadSpriteRefFromDecal entry with
                       | Some sprite -> yield (entry.Id, sprite)
                       | None -> () |]

        { Items = items; Fixtures = fixtures; Actors = actors; Decals = decals }

    // Convenience: load into the live EntityRegistry module (clears existing)
    let loadIntoModule (bytes: byte[]) =
        let data = deserialize bytes
        EntityRegistry.ItemProps.Clear()
        EntityRegistry.FixtureProps.Clear()
        EntityRegistry.ActorProps.Clear()
        EntityRegistry.DecalProps.Clear()

        for (id, p) in data.Items do EntityRegistry.ItemProps[id] <- p
        for (id, p) in data.Fixtures do EntityRegistry.FixtureProps[id] <- p
        for (id, p) in data.Actors do EntityRegistry.ActorProps[id] <- p
        for (id, s) in data.Decals do EntityRegistry.DecalProps[id] <- s