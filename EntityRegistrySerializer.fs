namespace AspectGameEngine
//Please fix Errors
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
        | TileOpacity.Translucent -> TileOpacityFBS.Translucent
        | _ -> TileOpacityFBS.Opaque

    let private fromTileOpacityFBS (opacityFBS: TileOpacityFBS) : TileOpacity =
        match opacityFBS with
        | TileOpacityFBS.Opaque -> TileOpacity.Opaque
        | TileOpacityFBS.Transparent -> TileOpacity.Transparent
        | TileOpacityFBS.Air -> TileOpacity.Air
        | TileOpacityFBS.Translucent -> TileOpacity.Translucent
        | _ -> TileOpacity.Opaque

    //=== SpriteRef helpers ===

    let private buildSpriteSheetCell (builder: FlatBufferBuilder) (c: SpriteSheetCell) =
        SpriteSheetCellFBS.CreateSpriteSheetCellFBS(builder, c.SheetId, c.Row, c.Column)

    let private buildSpriteSheetRegion (builder: FlatBufferBuilder) (r: SpriteSheetRegion) =
        SpriteSheetRegionFBS.CreateSpriteSheetRegionFBS(builder, r.SheetId, r.X, r.Y, r.Width, r.Height)

    let private buildSpriteSheetSpan (builder: FlatBufferBuilder) (s: SpriteSheetSpan) =
        let tl = buildSpriteSheetCell builder s.TopLeft
        SpriteSheetSpanFBS.CreateSpriteSheetSpanFBS(builder, tl, s.WidthCells, s.HeightCells)

    let private buildSpriteSheetCells (builder: FlatBufferBuilder) (cells: SpriteSheetCell[]) =
        let cellOffsets = cells |> Array.map (buildSpriteSheetCell builder)
        let cellsVec = SpriteSheetCellsFBS.CreateCellsVector(builder, cellOffsets)
        SpriteSheetCellsFBS.StartSpriteSheetCellsFBS(builder)
        SpriteSheetCellsFBS.AddCells(builder, cellsVec)
        SpriteSheetCellsFBS.EndSpriteSheetCellsFBS(builder)

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
        | SpriteRef.SheetSpan s ->
            let o = buildSpriteSheetSpan builder s
            (SpriteRefFBS.SpriteSheetSpanFBS, o.Value) 
        | SpriteRef.SheetCells cells ->
            let o = buildSpriteSheetCells builder cells
            (SpriteRefFBS.SpriteSheetCellsFBS, o.Value)

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
        | SpriteRefFBS.SpriteSheetSpanFBS ->
            match Option.ofNullable (ip.Sprite<SpriteSheetSpanFBS>()) with
            | None -> None
            | Some s ->
                match Option.ofNullable s.TopLeft with
                | None -> None
                | Some tl ->
                    Some(
                        SpriteRef.SheetSpan
                            { TopLeft = SpriteSheetCell(tl.SheetId, tl.Row, tl.Column)
                              WidthCells = s.WidthCells
                              HeightCells = s.HeightCells }
                    )
        | SpriteRefFBS.SpriteSheetCellsFBS ->
            match Option.ofNullable (ip.Sprite<SpriteSheetCellsFBS>()) with
            | None -> None
            | Some s ->
                let cells =
                    [| for i = 0 to s.CellsLength - 1 do
                           match Option.ofNullable (s.Cells(i)) with
                           | Some c -> yield SpriteSheetCell(c.SheetId, c.Row, c.Column)
                           | None -> () |]
                Some (SpriteRef.SheetCells cells)
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
        | SpriteRefFBS.SpriteSheetSpanFBS ->
            match Option.ofNullable (fp.Sprite<SpriteSheetSpanFBS>()) with
            | None -> None
            | Some s ->
                match Option.ofNullable s.TopLeft with
                | None -> None
                | Some tl ->
                    Some(
                        SpriteRef.SheetSpan
                            { TopLeft = SpriteSheetCell(tl.SheetId, tl.Row, tl.Column)
                              WidthCells = s.WidthCells
                              HeightCells = s.HeightCells }
                    )
        | SpriteRefFBS.SpriteSheetCellsFBS ->
            match Option.ofNullable (fp.Sprite<SpriteSheetCellsFBS>()) with
            | None -> None
            | Some s ->
                let cells =
                    [| for i = 0 to s.CellsLength - 1 do
                           match Option.ofNullable (s.Cells(i)) with
                           | Some c -> yield SpriteSheetCell(c.SheetId, c.Row, c.Column)
                           | None -> () |]
                Some (SpriteRef.SheetCells cells)
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

        | SpriteRefFBS.SpriteSheetSpanFBS ->
            match Option.ofNullable (ap.Sprite<SpriteSheetSpanFBS>()) with
            | None -> None
            | Some s ->
                match Option.ofNullable s.TopLeft with
                | None -> None
                | Some tl ->
                    Some(
                        SpriteRef.SheetSpan
                            { TopLeft = SpriteSheetCell(tl.SheetId, tl.Row, tl.Column)
                              WidthCells = s.WidthCells
                              HeightCells = s.HeightCells }
                    )
        | SpriteRefFBS.SpriteSheetCellsFBS ->
            match Option.ofNullable (ap.Sprite<SpriteSheetCellsFBS>()) with
            | None -> None
            | Some s ->
                let cells =
                    [| for i = 0 to s.CellsLength - 1 do
                           match Option.ofNullable (s.Cells(i)) with
                           | Some c -> yield SpriteSheetCell(c.SheetId, c.Row, c.Column)
                           | None -> () |]
                Some (SpriteRef.SheetCells cells)
        | _ -> None

    let private tryReadSpriteRefFromDecalProperties (dp: DecalPropertiesFBS) : SpriteRef option =
        match dp.SpriteType with
        | SpriteRefFBS.SpriteSheetRegionFBS ->
            match Option.ofNullable (dp.Sprite<SpriteSheetRegionFBS>()) with
            | Some s -> Some (SpriteRef.SheetRegion { SheetId = s.SheetId; X = s.X; Y = s.Y; Width = s.Width; Height = s.Height })
            | None -> None
        | SpriteRefFBS.SpriteSheetCellFBS ->
            match Option.ofNullable (dp.Sprite<SpriteSheetCellFBS>()) with 
            | Some s -> Some (SpriteRef.SheetCell(SpriteSheetCell(sheetId = s.SheetId, row = s.Row, column = s.Column )))
            | None -> None
        | SpriteRefFBS.TextureIdFBS ->
            match Option.ofNullable (dp.Sprite<TextureIdFBS>()) with
            | Some s -> Some (SpriteRef.TextureId s.Id)
            | None -> None
        | SpriteRefFBS.SceneRefFBS ->
            match Option.ofNullable (dp.Sprite<SceneRefFBS>()) with
            | Some s -> Some (SpriteRef.Scene s.Path)
            | None -> None
        | SpriteRefFBS.SpriteSheetSpanFBS ->
            match Option.ofNullable (dp.Sprite<SpriteSheetSpanFBS>()) with
            | None -> None
            | Some s ->
                match Option.ofNullable s.TopLeft with
                | None -> None
                | Some tl ->
                    Some(
                        SpriteRef.SheetSpan
                            { TopLeft = SpriteSheetCell(tl.SheetId, tl.Row, tl.Column)
                              WidthCells = s.WidthCells
                              HeightCells = s.HeightCells }
                    )
        | SpriteRefFBS.SpriteSheetCellsFBS ->
            match Option.ofNullable (dp.Sprite<SpriteSheetCellsFBS>()) with
            | None -> None
            | Some s ->
                let cells =
                    [| for i = 0 to s.CellsLength - 1 do
                           match Option.ofNullable (s.Cells(i)) with
                           | Some c -> yield SpriteSheetCell(c.SheetId, c.Row, c.Column)
                           | None -> () |]
                Some (SpriteRef.SheetCells cells)
        | _ -> None

    //=== Public data shape for deserialization ===

    type RegistryData =
        { SpriteProps: (int * SpriteProperties) array }

    //=== Serialization ===

    let serializeFrom (spriteProps: IDictionary<int, SpriteProperties>) : byte[] =
        let countEstimate = spriteProps.Count * 48 + 2048
        let builder = FlatBufferBuilder(max 2048 countEstimate)

        // Items
        let itemOffsets =
            [| for KeyValue(id, sp) in spriteProps do
                   match sp.SpriteType with
                   | SpriteType.Item (ip: ItemProperties) ->
                       let sType, sOff = buildSpriteRef builder sp.Sprite
                       let descOff = builder.CreateString(ip.DescKey)
                       ItemPropertiesFBS.StartItemPropertiesFBS(builder)
                       ItemPropertiesFBS.AddRenderLayer(builder, sp.RenderLayer)
                       ItemPropertiesFBS.AddDescKey(builder, descOff)
                       ItemPropertiesFBS.AddSpriteType(builder, sType)
                       ItemPropertiesFBS.AddSprite(builder, sOff)
                       let propsOff = ItemPropertiesFBS.EndItemPropertiesFBS(builder)

                       ItemPropsEntryFBS.StartItemPropsEntryFBS(builder)
                       ItemPropsEntryFBS.AddId(builder, id)
                       ItemPropsEntryFBS.AddProps(builder, propsOff)
                       yield ItemPropsEntryFBS.EndItemPropsEntryFBS(builder)
                   | _ -> () |]

        let itemsVec =
            if itemOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateItemsVector(builder, itemOffsets))

        // Fixtures
        let fixtureOffsets =
            [| for KeyValue(id, sp) in spriteProps do
                   match sp.SpriteType with
                   | SpriteType.Fixture fp ->
                       let sType, sOff = buildSpriteRef builder sp.Sprite
                       let descOff = builder.CreateString(fp.DescKey)
                       FixturePropertiesFBS.StartFixturePropertiesFBS(builder)
                       FixturePropertiesFBS.AddRenderLayer(builder, sp.RenderLayer)
                       FixturePropertiesFBS.AddBlocksMovement(builder, fp.BlocksMovement)
                       FixturePropertiesFBS.AddInteractable(builder, fp.Interactable)
                       FixturePropertiesFBS.AddMoveable(builder, fp.Moveable)
                       FixturePropertiesFBS.AddDescKey(builder, descOff)
                       FixturePropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS fp.TileOpacity)
                       FixturePropertiesFBS.AddSpriteType(builder, sType)
                       FixturePropertiesFBS.AddSprite(builder, sOff)
                       let propsOff = FixturePropertiesFBS.EndFixturePropertiesFBS(builder)

                       FixturePropsEntryFBS.StartFixturePropsEntryFBS(builder)
                       FixturePropsEntryFBS.AddId(builder, id)
                       FixturePropsEntryFBS.AddProps(builder, propsOff)
                       yield FixturePropsEntryFBS.EndFixturePropsEntryFBS(builder)
                   | _ -> () |]

        let fixturesVec =
            if fixtureOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateFixturesVector(builder, fixtureOffsets))
 
        let actorOffsets =
            [| for KeyValue(id, sp) in spriteProps do
                   match sp.SpriteType with
                   | SpriteType.Actor ap ->
                       let sType, sOff = buildSpriteRef builder sp.Sprite
                       let descOff = builder.CreateString(ap.DescKey)
                       ActorPropertiesFBS.StartActorPropertiesFBS(builder)
                       ActorPropertiesFBS.AddRenderLayer(builder, sp.RenderLayer)
                       ActorPropertiesFBS.AddTileOpacity(builder, toTileOpacityFBS ap.TileOpacity)
                       ActorPropertiesFBS.AddDescKey(builder, descOff)
                       ActorPropertiesFBS.AddSpriteType(builder, sType)
                       ActorPropertiesFBS.AddSprite(builder, sOff)
                       let propsOff = ActorPropertiesFBS.EndActorPropertiesFBS(builder)

                       ActorPropsEntryFBS.StartActorPropsEntryFBS(builder)
                       ActorPropsEntryFBS.AddId(builder, id)
                       ActorPropsEntryFBS.AddProps(builder, propsOff)
                       yield ActorPropsEntryFBS.EndActorPropsEntryFBS(builder)
                   | _ -> () |]

        let actorsVec =
            if actorOffsets.Length = 0 then Nullable()
            else Nullable(EntityRegistryFBS.CreateActorsVector(builder, actorOffsets))

        // Decals
        let decalOffsets =
            [| for KeyValue(id, sp) in spriteProps do
                   match sp.SpriteType with
                   | SpriteType.Decal dp ->
                       let sType, sOff = buildSpriteRef builder sp.Sprite
                       let descOff = builder.CreateString(dp.DescKey)
                       DecalPropertiesFBS.StartDecalPropertiesFBS(builder)
                       DecalPropertiesFBS.AddRenderLayer(builder, sp.RenderLayer)
                       DecalPropertiesFBS.AddInteractable(builder, dp.Interactable)
                       DecalPropertiesFBS.AddDescKey(builder, descOff)
                       DecalPropertiesFBS.AddSpriteType(builder, sType)
                       DecalPropertiesFBS.AddSprite(builder, sOff)
                       let propsOff = DecalPropertiesFBS.EndDecalPropertiesFBS(builder)

                       DecalPropsEntryFBS.StartDecalPropsEntryFBS(builder)
                       DecalPropsEntryFBS.AddId(builder, id)
                       DecalPropsEntryFBS.AddProps(builder, propsOff)
                       yield DecalPropsEntryFBS.EndDecalPropsEntryFBS(builder)
                   | _ -> () |]

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
        serializeFrom (EntityRegistry.SpriteProps :> IDictionary<_, _>)

    //=== Deserialization ===

    let deserialize (bytes: byte[]) : RegistryData =
        let bb = ByteBuffer(bytes)
        let root = EntityRegistryFBS.GetRootAsEntityRegistryFBS(bb)

        let out = ResizeArray<int * SpriteProperties>()

        // Items
        for i = 0 to root.ItemsLength - 1 do
            match Option.ofNullable (root.Items(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Props with
                | None -> ()
                | Some ipFbs ->
                    match tryReadSpriteRefFromItem ipFbs with
                    | None -> ()
                    | Some sprite ->
                        let ip = { DescKey = ipFbs.DescKey }
                        out.Add(entry.Id, { Sprite = sprite; SpriteType = SpriteType.Item ip; RenderLayer = ipFbs.RenderLayer })

        // Fixtures
        for i = 0 to root.FixturesLength - 1 do
            match Option.ofNullable (root.Fixtures(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Props with
                | None -> ()
                | Some fpFbs ->
                    match tryReadSpriteRefFromFixture fpFbs with
                    | None -> ()
                    | Some sprite ->
                        let fp =
                            { BlocksMovement = fpFbs.BlocksMovement
                              Interactable = fpFbs.Interactable
                              Moveable = fpFbs.Moveable
                              DescKey = fpFbs.DescKey
                              TileOpacity = fromTileOpacityFBS fpFbs.TileOpacity }
                        out.Add(entry.Id, { Sprite = sprite; SpriteType = SpriteType.Fixture fp; RenderLayer = fpFbs.RenderLayer })

        // Actors
        for i = 0 to root.ActorsLength - 1 do
            match Option.ofNullable (root.Actors(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Props with
                | None -> ()
                | Some apFbs ->
                    match tryReadSpriteRefFromActor apFbs with
                    | None -> ()
                    | Some sprite ->
                        let ap = { TileOpacity = fromTileOpacityFBS apFbs.TileOpacity; DescKey = apFbs.DescKey }
                        out.Add(entry.Id, { Sprite = sprite; SpriteType = SpriteType.Actor ap; RenderLayer = apFbs.RenderLayer })

        // Decals
        for i = 0 to root.DecalsLength - 1 do
            match Option.ofNullable (root.Decals(i)) with
            | None -> ()
            | Some entry ->
                match Option.ofNullable entry.Props with
                | None -> ()
                | Some dpFbs ->
                    match tryReadSpriteRefFromDecalProperties dpFbs with
                    | None -> ()
                    | Some sprite ->
                        let dp = { Interactable = dpFbs.Interactable; DescKey = dpFbs.DescKey }
                        out.Add(entry.Id, { Sprite = sprite; SpriteType = SpriteType.Decal dp; RenderLayer = dpFbs.RenderLayer })

        { SpriteProps = out.ToArray() }

    // Convenience: load into the live EntityRegistry module (clears existing)
    let loadIntoModule (bytes: byte[]) =
        let data = deserialize bytes
        EntityRegistry.SpriteProps.Clear()
        for (id, sp) in data.SpriteProps do
            EntityRegistry.SpriteProps[id] <- sp