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

let floorLoc = SpriteLoc(12, 0, 0)
let playerActorId = 12100
let barrelId = 12200
let decalLayer0Id = 12400
let decalLayer3Id = 12403

let registerTileset () =
    let props = TilePropertiesReference("DecalMovementReproSet")
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
    TilesetRegistry.register "DecalMovementReproSet" props

let registerEntities () =
    EntityRegistry.SpriteProps.[barrelId] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(12, 1, 0))
          SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = true; Moveable = 1; DescKey = "fixture.barrel"; TileOpacity = TileOpacity.Opaque }
          RenderLayer = 10 }
    EntityRegistry.SpriteProps.[decalLayer0Id] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(12, 2, 0))
          SpriteType = SpriteType.Decal { Interactable = false; DescKey = "decal.layer0" }
          RenderLayer = 0 }
    EntityRegistry.SpriteProps.[decalLayer3Id] <-
        { Sprite = SpriteRef.SheetCell(SpriteSheetCell(12, 2, 3))
          SpriteType = SpriteType.Decal { Interactable = false; DescKey = "decal.layer3" }
          RenderLayer = 3 }

let createVerticalMap (playerPos: GridPos) =
    registerTileset ()
    registerEntities ()
    let width, height = 1, 4
    let tiles =
        Array.init (width * height) (fun _ ->
            { SpriteLoc = floorLoc
              Health = 0
              IsOccupied = false })
    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, floorLoc, "DecalMovementReproSet", "DecalMovementRepro", MapType.Room)
    map.SpawnPoints.[0] <- (playerPos.X, playerPos.Y)
    map.InitEffectiveOpacityCache()
    map

let createModel map =
    GameUpdate.createWithPlayer map NpcFrames.Default playerActorId

let testPushUpWhileStandingOnProperLayer0Decal () =
    printfn "\n--- Repro: push barrel up while player stands on proper RenderLayer 0 decal ---"
    let map = createVerticalMap (GridPos(0, 2))
    map.SetFixture(0, 1, barrelId)
    map.AddDecal(0, 2, decalLayer0Id)
    let model = createModel map

    let result = Player.tryMove model 0 -1

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "barrel is pushed, not blocked"
    assertEquals (Some barrelId) (model.Map.TryGetFixture(0, 0)) "barrel moved up"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(0, 1)) "player moved into barrel old cell"
    assertEquals (Some decalLayer0Id) (model.Map.GetDecal(0, 2)) "decal remains under old player cell"

let testPushDownOntoProperLayer0Decal () =
    printfn "\n--- Repro: push barrel down onto proper RenderLayer 0 decal ---"
    let map = createVerticalMap (GridPos(0, 1))
    map.SetFixture(0, 2, barrelId)
    map.AddDecal(0, 3, decalLayer0Id)
    let model = createModel map

    let result = Player.tryMove model 0 1

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "barrel is pushed onto decal, not swapped"
    assertEquals (Some barrelId) (model.Map.TryGetFixture(0, 3)) "barrel moved onto decal cell"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(0, 2)) "player moved into barrel old cell"
    assertEquals (Some decalLayer0Id) (model.Map.GetDecal(0, 3)) "decal remains at barrel destination"

let testPushDownOntoProperLayer3Decal () =
    printfn "\n--- Repro: push barrel down onto proper RenderLayer 3 decal ---"
    let map = createVerticalMap (GridPos(0, 1))
    map.SetFixture(0, 2, barrelId)
    map.AddDecal(0, 3, decalLayer3Id)
    let model = createModel map

    let result = Player.tryMove model 0 1

    assertTrue result.Succeeded "movement succeeds"
    assertEquals MovementKind.PushedMoveable result.Kind "barrel is pushed onto layer 3 decal"
    assertEquals (Some barrelId) (model.Map.TryGetFixture(0, 3)) "barrel moved onto decal cell"

let testCorruptedSourceCellFixtureSlotReproducesBlockedMovement () =
    printfn "\n--- Repro: decal id misrouted into player's FixtureId reproduces blocked push ---"
    let map = createVerticalMap (GridPos(0, 2))
    map.SetFixture(0, 1, barrelId)
    map.SetFixture(0, 2, decalLayer0Id)
    let model = createModel map

    let result = Player.tryMove model 0 -1

    assertTrue (not result.Succeeded) "movement fails when player source cell also has fixture slot"
    assertEquals (Some MovementBlockedCause.SwapBlocked) result.BlockedCause "failed as swap blocked"
    assertEquals (Some barrelId) (model.Map.TryGetFixture(0, 1)) "barrel stays put"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(0, 2)) "player stays put"

let testCorruptedPushDestinationFixtureSlotReproducesSwap () =
    printfn "\n--- Repro: decal id misrouted into push destination FixtureId reproduces unexpected swap ---"
    let map = createVerticalMap (GridPos(0, 1))
    map.SetFixture(0, 2, barrelId)
    map.SetFixture(0, 3, decalLayer0Id)
    let model = createModel map

    let result = Player.tryMove model 0 1

    assertTrue result.Succeeded "movement succeeds through swap fallback"
    assertEquals MovementKind.SwappedMoveable result.Kind "barrel swaps instead of pushing onto decal"
    assertEquals (Some barrelId) (model.Map.TryGetFixture(0, 1)) "barrel moved into player old cell"
    assertEquals (Some playerActorId) (model.Map.TryGetActor(0, 2)) "player moved into barrel old cell"
    assertEquals (Some decalLayer0Id) (model.Map.TryGetFixture(0, 3)) "misrouted decal remains in fixture slot"

testPushUpWhileStandingOnProperLayer0Decal ()
testPushDownOntoProperLayer0Decal ()
testPushDownOntoProperLayer3Decal ()
testCorruptedSourceCellFixtureSlotReproducesBlockedMovement ()
testCorruptedPushDestinationFixtureSlotReproducesSwap ()

printfn "\n=== Decal movement repro complete ==="
