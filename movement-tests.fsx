#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"

open AspectGameEngine

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let assertTrue cond message =
    if not cond then failwithf "ASSERTION FAILED: %s" message
    else printfn "PASSED: %s" message

let floorLoc = SpriteLoc(9, 0, 0)
let wallLoc = SpriteLoc(9, 0, 1)
let doorClosedLoc = SpriteLoc(9, 0, 2)
let doorOpenLoc = SpriteLoc(9, 0, 3)
let playerActorId = 9100
let otherActorId = 9101
let moveableFixtureId = 9200
let heavyFixtureId = 9201
let blockedFixtureId = 9202
let transparentMoveableFixtureId = 9203
let emptyDescFixtureId = 9204
let itemId = 9300
let decalId = 9400

let registerTileset () =
    let props = TilePropertiesReference("MovementTestSet")
    props.[floorLoc] <-
        { Walkable = true
          Interactable = false
          TileType = TileType.Floor
          Health = 0
          DescriptionKey = "tile.floor"
          Biome = Biome.None
          TileOpacity = TileOpacity.Transparent
          Visuals = [||]
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          ComplexState = None }
    props.[wallLoc] <-
        { Walkable = false
          Interactable = false
          TileType = TileType.Wall
          Health = 0
          DescriptionKey = "tile.wall"
          Biome = Biome.None
          TileOpacity = TileOpacity.Opaque
          Visuals = [||]
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          ComplexState = None }
    props.[doorClosedLoc] <-
        { Walkable = true
          Interactable = true
          TileType = TileType.Door
          Health = 0
          DescriptionKey = "tile.door.closed"
          Biome = Biome.None
          TileOpacity = TileOpacity.Opaque
          Visuals = [| { Key = "door-opened"; SpriteLoc = doorOpenLoc } |]
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          ComplexState = None }
    props.[doorOpenLoc] <-
        { Walkable = true
          Interactable = true
          TileType = TileType.Door
          Health = 0
          DescriptionKey = "tile.door.open"
          Biome = Biome.None
          TileOpacity = TileOpacity.Transparent
          Visuals = [| { Key = "door-closed"; SpriteLoc = doorClosedLoc } |]
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          ComplexState = None }
    TilesetRegistry.register "MovementTestSet" props

let registerEntities () =
    EntityRegistry.SpriteProps.[moveableFixtureId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 1, 0))
          SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = true; Moveable = 1; DescKey = "fixture.moveable"; TileOpacity = TileOpacity.Opaque }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[heavyFixtureId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 1, 1))
          SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = true; Moveable = 2; DescKey = "fixture.heavy"; TileOpacity = TileOpacity.Opaque }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[blockedFixtureId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 1, 2))
          SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = true; Moveable = 0; DescKey = "fixture.blocked"; TileOpacity = TileOpacity.Opaque }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[transparentMoveableFixtureId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 1, 3))
          SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = true; Moveable = 1; DescKey = "fixture.transparent-moveable"; TileOpacity = TileOpacity.Transparent }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[emptyDescFixtureId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 1, 4))
          SpriteType = SpriteType.Fixture { BlocksMovement = false; Interactable = false; Moveable = 0; DescKey = ""; TileOpacity = TileOpacity.Transparent }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[itemId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 2, 0))
          SpriteType = SpriteType.Item { DescKey = "item.test" }
          RenderLayer = 30 }
    EntityRegistry.SpriteProps.[decalId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(9, 3, 0))
          SpriteType = SpriteType.Decal { Interactable = false; DescKey = "decal.test" }
          RenderLayer = 5 }

let createMap (wallAt: GridPos option) (fixtureAt: (GridPos * int) option) (playerSpawn: GridPos) =
    registerTileset ()
    registerEntities ()

    let width, height = 5, 3
    let tiles =
        Array.init (width * height) (fun idx ->
            let x = idx % width
            let y = idx / width
            let spriteLoc =
                match wallAt with
                | Some p when p.X = x && p.Y = y -> wallLoc
                | _ -> floorLoc
            { SpriteLoc = spriteLoc
              Health = 0
              IsOccupied = false })
    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, floorLoc, "MovementTestSet", "MovementTest", MapType.Room)
    map.SpawnPoints.[0] <- (playerSpawn.X, playerSpawn.Y)
    map.InitEffectiveOpacityCache()
    match fixtureAt with
    | Some (pos, fixtureId) -> map.SetFixture(pos.X, pos.Y, fixtureId)
    | None -> ()
    map

let createDefaultMap () =
    createMap None None (GridPos(1, 1))

let createDoorMap () =
    registerTileset ()
    registerEntities ()

    let width, height = 5, 3
    let tiles =
        Array.init (width * height) (fun idx ->
            let x = idx % width
            let y = idx / width
            let spriteLoc = if x = 2 && y = 1 then doorClosedLoc else floorLoc
            { SpriteLoc = spriteLoc
              Health = 0
              IsOccupied = false })
    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, floorLoc, "MovementTestSet", "MovementTest", MapType.Room)
    map.SpawnPoints.[0] <- (1, 1)
    map.InitEffectiveOpacityCache()
    map

let createDoorMapWithDoorState (doorSpriteLoc: SpriteLoc) (closedDescKey: string) (openDescKey: string) =
    registerTileset ()
    registerEntities ()

    let props = TilesetRegistry.get "MovementTestSet"
    props.[doorClosedLoc] <- { props.[doorClosedLoc] with DescriptionKey = closedDescKey }
    props.[doorOpenLoc] <- { props.[doorOpenLoc] with DescriptionKey = openDescKey }

    let width, height = 5, 3
    let tiles =
        Array.init (width * height) (fun idx ->
            let x = idx % width
            let y = idx / width
            let spriteLoc = if x = 2 && y = 1 then doorSpriteLoc else floorLoc
            { SpriteLoc = spriteLoc
              Health = 0
              IsOccupied = false })
    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, floorLoc, "MovementTestSet", "MovementTest", MapType.Room)
    map.SpawnPoints.[0] <- (1, 1)
    map.InitEffectiveOpacityCache()
    map

let createModel map =
    GameUpdate.createWithPlayer map NpcFrames.Default playerActorId

let snapshotVisibility (model: GameModel) =
    [|
        for y in 0 .. model.Map.Height - 1 do
            for x in 0 .. model.Map.Width - 1 do
                yield
                    (GridPos(x, y),
                     RectFov.isVisible model.VisibilityState model.Map x y,
                     RectFov.isDisclosed model.VisibilityState model.Map x y,
                     RectFov.isPresentedVisible model.VisibilityState model.Map x y,
                     RectFov.translucencyDepth model.VisibilityState model.Map x y,
                     RectFov.presentedDepth model.VisibilityState model.Map x y,
                     RectFov.isExplored model.VisibilityState model.Map x y)
    |]

let assertVisibilityUnchanged before model message =
    assertEquals before (snapshotVisibility model) message

let assertChangeSetEmpty (changes: EngineChangeSet) message =
    assertEquals [||] changes.ChangedBaseCells (message + " changed base cells")
    assertEquals [||] changes.ChangedLayerCells (message + " changed layer cells")
    assertEquals [||] changes.ChangedEntities (message + " changed entities")
    assertEquals false changes.VisibilityInputChanged (message + " visibility flag")
    assertEquals false changes.OcclusionInputChanged (message + " occlusion flag")
    assertEquals false changes.SaveRelevant (message + " save flag")

let testCreateLeavesVisibilityCacheEmpty () =
    printfn "\n--- Test: create leaves visibility cache empty ---"
    let model = createDefaultMap () |> createModel

    assertEquals false (RectFov.isVisible model.VisibilityState model.Map 1 1) "player tile is not visible before explicit recompute"
    assertEquals false (RectFov.isPresentedVisible model.VisibilityState model.Map 1 1) "player tile is not presentation-visible before explicit recompute"
    assertEquals false (RectFov.isExplored model.VisibilityState model.Map 1 1) "player tile is not explored before explicit recompute"

    GameUpdate.recomputeVisibility model

    assertEquals true (RectFov.isVisible model.VisibilityState model.Map 1 1) "player tile is visible after explicit recompute"
    assertEquals true (RectFov.isPresentedVisible model.VisibilityState model.Map 1 1) "player tile is presentation-visible after explicit recompute"
    assertEquals true (RectFov.isExplored model.VisibilityState model.Map 1 1) "player tile is explored after explicit recompute"

let testVisibilitySettersDoNotRecompute () =
    printfn "\n--- Test: visibility setters do not recompute ---"
    let model = createDefaultMap () |> createModel
    let before = snapshotVisibility model

    GameUpdate.setVisibilityWindow model 0 0
    assertEquals 0 model.VisibilityHalfWidth "visibility half-width updated"
    assertEquals 0 model.VisibilityHalfHeight "visibility half-height updated"
    assertVisibilityUnchanged before model "visibility window setter leaves cache untouched"

    GameUpdate.setVisibilityTranslucencyBudget model 2
    assertEquals 2 model.VisibilityTranslucencyBudget "visibility translucency budget updated"
    assertVisibilityUnchanged before model "visibility budget setter leaves cache untouched"

let testMovementLeavesVisibilityCacheStaleUntilExplicitRecompute () =
    printfn "\n--- Test: movement leaves visibility cache stale until explicit recompute ---"
    let model = createDefaultMap () |> createModel
    GameUpdate.setVisibilityWindow model 0 0
    GameUpdate.recomputeVisibility model
    let before = snapshotVisibility model

    assertEquals true (RectFov.isVisible model.VisibilityState model.Map 1 1) "old player tile visible before move"
    assertEquals false (RectFov.isVisible model.VisibilityState model.Map 2 1) "new player tile not visible before move"

    let result = Player.tryMove model 1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals true result.Changes.VisibilityInputChanged "movement reports visibility input change"
    assertVisibilityUnchanged before model "movement does not eagerly refresh visibility cache"

    GameUpdate.recomputeVisibility model

    assertEquals false (RectFov.isVisible model.VisibilityState model.Map 1 1) "old player tile no longer visible after explicit recompute"
    assertEquals true (RectFov.isVisible model.VisibilityState model.Map 2 1) "new player tile visible after explicit recompute"

let testInteractionLeavesVisibilityCacheStaleUntilExplicitRecompute () =
    printfn "\n--- Test: interaction leaves visibility cache stale until explicit recompute ---"
    let model = createDoorMap () |> createModel
    Player.setFacing model ActorFacing.Right
    GameUpdate.setVisibilityWindow model 3 1
    GameUpdate.recomputeVisibility model
    let before = snapshotVisibility model

    assertEquals false (RectFov.isVisible model.VisibilityState model.Map 3 1) "closed opaque door blocks tile beyond before interaction"

    let succeeded, result = GameUpdate.tryForInteractionsResult model

    assertTrue succeeded "auto interaction succeeds"
    assertTrue result.Succeeded "door result succeeds"
    assertEquals true result.Changes.VisibilityInputChanged "door interaction reports visibility input change"
    assertVisibilityUnchanged before model "door interaction does not eagerly refresh visibility cache"

    GameUpdate.recomputeVisibility model

    assertEquals true (RectFov.isVisible model.VisibilityState model.Map 3 1) "tile beyond open door visible after explicit recompute"

let testNormalMoveReportsChanges () =
    printfn "\n--- Test: normal move reports changes ---"
    let model = createDefaultMap () |> createModel
    let result = Player.tryMove model 1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.Normal result.Kind "normal movement kind"
    assertEquals (Some "movement.moved") (result.Message |> Option.map (fun m -> m.Key)) "normal movement message key"
    assertEquals (GridPos(1, 1)) result.PlayerOldPosition "old player position"
    assertEquals (GridPos(2, 1)) result.PlayerNewPosition "new player position"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(2, 1)) "actor moved to destination"
    assertEquals None (model.Map.TryGetActor(1, 1)) "actor left source"
    assertEquals [||] result.Changes.ChangedBaseCells "normal movement changed no base cells"
    assertEquals [| GridPos(1, 1); GridPos(2, 1) |] result.Changes.ChangedLayerCells "changed layer cells"
    assertEquals
        [| { Position = GridPos(1, 1); Slot = ChangeSlot.Actor; EntityId = None; LocalObjectId = None }
           { Position = GridPos(2, 1); Slot = ChangeSlot.Actor; EntityId = Some playerActorId; LocalObjectId = None } |]
        result.Changes.ChangedEntities
        "normal movement changed actor entities"
    assertEquals true result.Changes.VisibilityInputChanged "normal movement refreshes visibility because player position changed"
    assertEquals false result.Changes.OcclusionInputChanged "normal transparent actor move does not change occlusion inputs"
    assertEquals true result.Changes.SaveRelevant "normal movement is save relevant"

let testMoveablePushForward () =
    printfn "\n--- Test: moveable pushes forward ---"
    let model =
        createMap None (Some (GridPos(2, 1), moveableFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "pushed moveable kind"
    assertEquals (Some "movement.moveable.pushed") (result.Message |> Option.map (fun m -> m.Key)) "pushed moveable message key"
    assertEquals (GridPos(2, 1)) result.PlayerNewPosition "player enters fixture old cell"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(2, 1)) "actor moved into fixture old cell"
    assertEquals (Some moveableFixtureId) (model.Map.TryGetFixture(3, 1)) "fixture pushed forward"
    assertEquals None (model.Map.TryGetFixture(2, 1)) "fixture left old cell"
    assertEquals TileOpacity.Transparent (model.Map.GetOpacity(2, 1)) "fixture old cell opacity cleared to actor opacity"
    assertEquals TileOpacity.Opaque (model.Map.GetOpacity(3, 1)) "fixture new cell opacity applied"
    assertEquals (Some { Slot = ChangeSlot.Fixture; EntityId = Some moveableFixtureId; LocalObjectId = None; OldPosition = GridPos(2, 1); NewPosition = GridPos(3, 1) }) result.MovedObject "moved fixture result"
    assertEquals [| GridPos(1, 1); GridPos(2, 1); GridPos(3, 1) |] result.Changes.ChangedLayerCells "changed push cells"
    assertEquals
        [| { Position = GridPos(1, 1); Slot = ChangeSlot.Actor; EntityId = None; LocalObjectId = None }
           { Position = GridPos(2, 1); Slot = ChangeSlot.Actor; EntityId = Some playerActorId; LocalObjectId = None }
           { Position = GridPos(2, 1); Slot = ChangeSlot.Fixture; EntityId = None; LocalObjectId = None }
           { Position = GridPos(3, 1); Slot = ChangeSlot.Fixture; EntityId = Some moveableFixtureId; LocalObjectId = None } |]
        result.Changes.ChangedEntities
        "pushed moveable changed actor and fixture entities"
    assertEquals true result.Changes.VisibilityInputChanged "pushed moveable refreshes visibility"
    assertEquals true result.Changes.OcclusionInputChanged "opaque moveable push changes occlusion inputs"
    assertEquals true result.Changes.SaveRelevant "pushed moveable movement is save relevant"

let testMoveableSwapsWhenPushBlocked () =
    printfn "\n--- Test: moveable swaps when push blocked ---"
    let model =
        createMap
            (Some (GridPos(3, 1)))
            (Some (GridPos(2, 1), moveableFixtureId))
            (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.SwappedMoveable result.Kind "swapped moveable kind"
    assertEquals (Some "movement.moveable.swapped") (result.Message |> Option.map (fun m -> m.Key)) "swapped moveable message key"
    assertEquals (GridPos(2, 1)) result.PlayerNewPosition "player enters fixture old cell"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(2, 1)) "actor swapped into fixture old cell"
    assertEquals (Some moveableFixtureId) (model.Map.TryGetFixture(1, 1)) "fixture swapped into player old cell"
    assertEquals None (model.Map.TryGetFixture(2, 1)) "fixture left old cell"
    assertEquals (Some { Slot = ChangeSlot.Fixture; EntityId = Some moveableFixtureId; LocalObjectId = None; OldPosition = GridPos(2, 1); NewPosition = GridPos(1, 1) }) result.MovedObject "moved fixture result"
    assertEquals true result.Changes.VisibilityInputChanged "swapped moveable refreshes visibility"
    assertEquals true result.Changes.OcclusionInputChanged "opaque swap moves the occlusion footprint"
    assertEquals true result.Changes.SaveRelevant "swapped moveable movement is save relevant"

let testTransparentMoveableDoesNotReportOcclusionChange () =
    printfn "\n--- Test: transparent moveable push does not report occlusion change ---"
    let model =
        createMap None (Some (GridPos(2, 1), transparentMoveableFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "transparent moveable pushed"
    assertEquals true result.Changes.VisibilityInputChanged "transparent moveable still refreshes visibility because player moved"
    assertEquals false result.Changes.OcclusionInputChanged "transparent moveable does not change occlusion inputs"
    assertEquals true result.Changes.SaveRelevant "transparent moveable movement is save relevant"

let testMoveableSwapsWhenPushOutOfBounds () =
    printfn "\n--- Test: moveable swaps when push destination is out of bounds ---"
    let model =
        createMap None (Some (GridPos(0, 1), moveableFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model -1 0

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.SwappedMoveable result.Kind "swapped moveable kind"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(0, 1)) "actor swapped into fixture old cell"
    assertEquals (Some moveableFixtureId) (model.Map.TryGetFixture(1, 1)) "fixture swapped into player old cell"
    assertEquals (Some { Slot = ChangeSlot.Fixture; EntityId = Some moveableFixtureId; LocalObjectId = None; OldPosition = GridPos(0, 1); NewPosition = GridPos(1, 1) }) result.MovedObject "moved fixture result"

let testMoveablePushesDiagonally () =
    printfn "\n--- Test: moveable pushes diagonally ---"
    let model =
        createMap None (Some (GridPos(2, 1), moveableFixtureId)) (GridPos(1, 2))
        |> createModel

    let result = Player.tryMove model 1 -1

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "pushed moveable kind"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(2, 1)) "actor moved into fixture old cell"
    assertEquals (Some moveableFixtureId) (model.Map.TryGetFixture(3, 0)) "fixture pushed diagonally"
    assertEquals (Some { Slot = ChangeSlot.Fixture; EntityId = Some moveableFixtureId; LocalObjectId = None; OldPosition = GridPos(2, 1); NewPosition = GridPos(3, 0) }) result.MovedObject "moved fixture result"

let testOutOfBoundsBlocks () =
    printfn "\n--- Test: out of bounds blocks ---"
    let model = createDefaultMap () |> createModel
    let result = Player.tryMove model -2 0

    assertTrue (not result.Succeeded) "movement fails"
    assertEquals (Some MovementBlockedCause.DestinationOutOfBounds) result.BlockedCause "blocked by bounds"
    assertEquals (Some "movement.blocked.destination-out-of-bounds") (result.Message |> Option.map (fun m -> m.Key)) "bounds blocked message key"
    assertEquals (GridPos(1, 1)) model.PlayerModel.PlayerPos "player stays put"
    assertChangeSetEmpty result.Changes "out-of-bounds block"

let testWallBlocks () =
    printfn "\n--- Test: wall blocks ---"
    let model =
        createMap (Some (GridPos(2, 1))) None (GridPos(1, 1))
        |> createModel
    let result = Player.tryMove model 1 0

    assertTrue (not result.Succeeded) "movement fails"
    assertEquals (Some MovementBlockedCause.DestinationNotWalkable) result.BlockedCause "blocked by wall"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(1, 1)) "actor stays put"
    assertChangeSetEmpty result.Changes "wall block"

let testActorBlocks () =
    printfn "\n--- Test: actor blocks ---"
    let map = createDefaultMap ()
    map.SetActor(2, 1, otherActorId)
    let model = createModel map
    let result = Player.tryMove model 1 0

    assertTrue (not result.Succeeded) "movement fails"
    assertEquals (Some MovementBlockedCause.DestinationOccupiedByActor) result.BlockedCause "blocked by actor"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(1, 1)) "player actor stays put"
    assertEquals (Some otherActorId) (model.Map.TryGetActor(2, 1)) "blocking actor stays put"
    assertChangeSetEmpty result.Changes "actor block"

let testNonMoveableFixtureBlocks () =
    printfn "\n--- Test: non-moveable fixture blocks ---"
    let model =
        createMap None (Some (GridPos(2, 1), blockedFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0

    assertTrue (not result.Succeeded) "movement fails"
    assertEquals (Some MovementBlockedCause.DestinationBlockedByFixture) result.BlockedCause "blocked by fixture"
    assertEquals (GridPos(1, 1)) model.PlayerModel.PlayerPos "player stays put"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(1, 1)) "actor stays put"
    assertEquals (Some blockedFixtureId) (model.Map.TryGetFixture(2, 1)) "fixture stays put"
    assertChangeSetEmpty result.Changes "non-moveable fixture block"

let testHeavyMoveableRequiresStrength () =
    printfn "\n--- Test: heavy moveable requires strength ---"
    let model =
        createMap None (Some (GridPos(2, 1), heavyFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0

    assertTrue (not result.Succeeded) "movement fails"
    assertEquals (Some MovementBlockedCause.MoveableRequiresStrength) result.BlockedCause "requires strength"
    assertEquals (GridPos(1, 1)) model.PlayerModel.PlayerPos "player stays put"
    assertEquals (Some heavyFixtureId) (model.Map.TryGetFixture(2, 1)) "fixture stays put"
    assertChangeSetEmpty result.Changes "heavy moveable block"

let testDoorInteractionResultReportsChangedBaseCell () =
    printfn "\n--- Test: door interaction result reports changed base cell ---"
    let model = createDoorMap () |> createModel
    let doorPos = GridPos(2, 1)

    let result = GameUpdate.interactAtResult model doorPos (InteractionType.DoorInteraction DoorAction.OpenOrCloseDoor)

    assertTrue result.Succeeded "door interaction succeeds"
    assertEquals (Some "door-opened") (result.Message |> Option.map (fun m -> m.Key)) "door interaction message key"
    assertEquals (Some doorPos) result.TargetPosition "door interaction target position"
    assertEquals (Some ChangeSlot.BaseTile) result.TargetSlot "door interaction target slot"
    assertEquals [| doorPos |] result.Changes.ChangedBaseCells "door changed base cell"
    assertEquals [||] result.Changes.ChangedLayerCells "door changed no layer cells"
    assertEquals
        [| { Position = doorPos; Slot = ChangeSlot.BaseTile; EntityId = None; LocalObjectId = None } |]
        result.Changes.ChangedEntities
        "door changed base tile entity"
    assertEquals true result.Changes.VisibilityInputChanged "opaque door opening changes visibility inputs"
    assertEquals true result.Changes.OcclusionInputChanged "opaque door opening changes occlusion inputs"
    assertEquals true result.Changes.SaveRelevant "door interaction is save relevant"

let testDoorAutoOpenIgnoresDescriptionKey () =
    printfn "\n--- Test: door auto-open ignores description key ---"
    let model =
        createDoorMapWithDoorState doorClosedLoc "tile.door.open" "tile.door.custom-open"
        |> createModel

    let result = Player.tryMove model 1 0
    let doorTile = model.Map.GetTile(2, 1)

    assertTrue result.Succeeded "movement into closed door succeeds"
    assertEquals doorOpenLoc doorTile.SpriteLoc "closed door auto-opens even when its description key looks open"
    assertEquals [| GridPos(2, 1) |] result.Changes.ChangedBaseCells "auto-open reports changed base cell"

let testDoorAutoOpenDoesNotToggleOpenDoor () =
    printfn "\n--- Test: door auto-open does not toggle open door ---"
    let model =
        createDoorMapWithDoorState doorOpenLoc "tile.door.closed-renamed" "tile.door.open-renamed"
        |> createModel

    let result = Player.tryMove model 1 0
    let doorTile = model.Map.GetTile(2, 1)

    assertTrue result.Succeeded "movement into open door succeeds"
    assertEquals doorOpenLoc doorTile.SpriteLoc "open door remains open during movement"
    assertEquals [||] result.Changes.ChangedBaseCells "open door movement reports no base-cell change"
    assertEquals false result.Changes.OcclusionInputChanged "open transparent door does not change occlusion inputs"

let testLookAtReturnsDescriptionKeys () =
    printfn "\n--- Test: lookAt returns base and entity description keys ---"
    let map = createDefaultMap ()
    map.SetFixture(2, 1, moveableFixtureId)
    assertTrue (map.AddItem(2, 1, itemId)) "item placed for look test"
    map.AddDecal(2, 1, decalId)

    let model = GameUpdate.create map
    let info = GameUpdate.lookAt model (GridPos(2, 1))

    assertEquals LookAtResult.MultipleObjectsFullyListed info.Result "look result lists all described layer objects"
    assertEquals 3uy info.LayerCount "look layer key count"
    assertEquals false info.HasMore "look has no undisplayed layer object"
    assertEquals "item.test" info.LayerKey0 "top item description key"
    assertEquals "fixture.moveable" info.LayerKey1 "fixture description key"
    assertEquals "decal.test" info.LayerKey2 "decal description key"
    assertEquals "tile.floor" info.BaseKey "base tile description key"

let testLookAtMissingEntityDescriptionFallsBackToBaseKey () =
    printfn "\n--- Test: lookAt missing entity description falls back to base key ---"
    let map = createDefaultMap ()
    map.SetFixture(2, 1, emptyDescFixtureId)

    let model = GameUpdate.create map
    let info = GameUpdate.lookAt model (GridPos(2, 1))

    assertEquals LookAtResult.SingleObject info.Result "base key still describes tile"
    assertEquals 0uy info.LayerCount "empty entity description key is not returned"
    assertEquals true info.HasMore "missing entity description is visible through HasMore"
    assertEquals "" info.LayerKey0 "missing layer key remains empty"
    assertEquals "tile.floor" info.BaseKey "base key remains available"

let testMoveableStateRoundTripsAfterSerialization () =
    printfn "\n--- Test: moved actor and fixture round-trip through map serialization ---"
    let model =
        createMap None (Some (GridPos(2, 1), moveableFixtureId)) (GridPos(1, 1))
        |> createModel

    let result = Player.tryMove model 1 0
    assertTrue result.Succeeded "movement succeeds before serialization"
    assertEquals MovementKind.PushedMoveable result.Kind "pushed moveable before serialization"

    let bytes = TileMapSerializer.serialize model.Map
    let deserializedMap = TileMapSerializer.deserialize bytes

    assertEquals (Some playerActorId) (deserializedMap.TryGetActor(2, 1)) "player actor position preserved"
    assertEquals (Some moveableFixtureId) (deserializedMap.TryGetFixture(3, 1)) "moved fixture position preserved"
    assertEquals None (deserializedMap.TryGetActor(1, 1)) "old player actor position remains empty"
    assertEquals None (deserializedMap.TryGetFixture(2, 1)) "old fixture position remains empty"

testNormalMoveReportsChanges ()
testCreateLeavesVisibilityCacheEmpty ()
testVisibilitySettersDoNotRecompute ()
testMovementLeavesVisibilityCacheStaleUntilExplicitRecompute ()
testInteractionLeavesVisibilityCacheStaleUntilExplicitRecompute ()
testMoveablePushForward ()
testMoveableSwapsWhenPushBlocked ()
testTransparentMoveableDoesNotReportOcclusionChange ()
testMoveableSwapsWhenPushOutOfBounds ()
testMoveablePushesDiagonally ()
testOutOfBoundsBlocks ()
testWallBlocks ()
testActorBlocks ()
testNonMoveableFixtureBlocks ()
testHeavyMoveableRequiresStrength ()
testDoorInteractionResultReportsChangedBaseCell ()
testDoorAutoOpenIgnoresDescriptionKey ()
testDoorAutoOpenDoesNotToggleOpenDoor ()
testLookAtReturnsDescriptionKeys ()
testLookAtMissingEntityDescriptionFallsBackToBaseKey ()
testMoveableStateRoundTripsAfterSerialization ()

printfn "\n=== Movement tests passed ==="
