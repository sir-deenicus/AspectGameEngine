#r "bin/Debug/net8.0/AspectGameEngine.dll"

open System
open AspectGameEngine

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let assertTrue cond message =
    if not cond then failwithf "ASSERTION FAILED: %s" message
    else printfn "PASSED: %s" message

let assertFalse cond message =
    assertTrue (not cond) message

let tilesetName = "FovTests_" + Guid.NewGuid().ToString("N")
let floorLoc = SpriteLoc(0, 0, 1)
let wallLoc = SpriteLoc(0, 0, 2)
let glassLoc = SpriteLoc(0, 0, 3)
let bookshelfFixtureId = 910001

let registerTileset () =
    let props = TilePropertiesReference(tilesetName)

    props.[floorLoc] <-
        { TileProperties.NullTile with
            Walkable = true
            TileType = TileType.Floor
            TileOpacity = TileOpacity.Transparent }

    props.[wallLoc] <-
        { TileProperties.NullTile with
            Walkable = false
            TileType = TileType.Wall
            TileOpacity = TileOpacity.Opaque }

    props.[glassLoc] <-
        { TileProperties.NullTile with
            Walkable = true
            TileType = TileType.Wall
            TileOpacity = TileOpacity.Translucent }

    TilesetRegistry.register tilesetName props

let registerFixtures () =
    EntityRegistry.SpriteProps.[bookshelfFixtureId] <-
        { Sprite = SpriteRef.TextureId(bookshelfFixtureId)
          SpriteType =
            SpriteType.Fixture
                { BlocksMovement = true
                  Interactable = false
                  Moveable = 0
                  DescKey = "bookshelf"
                  TileOpacity = TileOpacity.Opaque }
          RenderLayer = 10 }

registerTileset ()
registerFixtures ()

let tileForChar ch =
    let loc =
        match ch with
        | '#' -> wallLoc
        | 'G' -> glassLoc
        | _ -> floorLoc

    { SpriteLoc = loc
      Health = 0
      IsOccupied = false }

let findOrigin (rows: string[]) =
    rows
    |> Array.mapi (fun y row ->
        row.ToCharArray()
        |> Array.tryFindIndex ((=) '@')
        |> Option.map (fun x -> GridPos(x, y)))
    |> Array.choose id
    |> function
        | [| origin |] -> origin
        | [||] -> failwith "Test map must contain an origin marked with '@'."
        | _ -> failwith "Test map must contain exactly one origin marked with '@'."

let makeMap (rows: string[]) =
    if rows.Length = 0 then failwith "Test map must have at least one row."

    let width = rows.[0].Length
    if width = 0 then failwith "Test map rows must not be empty."

    for row in rows do
        if row.Length <> width then
            failwithf "Test map rows must be rectangular. Expected width %d but found %d in '%s'." width row.Length row

    let height = rows.Length
    let tiles =
        rows
        |> Array.collect (fun row -> row.ToCharArray() |> Array.map tileForChar)

    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, wallLoc, tilesetName, "fov-test", MapType.Room)
    map.InitEffectiveOpacityCache()

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            if rows.[y].[x] = 'B' then
                map.SetFixture(x, y, bookshelfFixtureId)

    map

let compute (rows: string[]) budget =
    let map = makeMap rows
    let state = VisibilityState(map.Width, map.Height)
    let origin = findOrigin rows
    RectFov.compute map state origin map.Width map.Height budget
    map, state, origin

let rowsFromPoints width height origin walls glass fixtures =
    let wallSet = Set.ofList walls
    let glassSet = Set.ofList glass
    let fixtureSet = Set.ofList fixtures

    Array.init height (fun y ->
        Array.init width (fun x ->
            if x = fst origin && y = snd origin then '@'
            elif Set.contains (x, y) fixtureSet then 'B'
            elif Set.contains (x, y) glassSet then 'G'
            elif Set.contains (x, y) wallSet then '#'
            else '.')
        |> String)

let isVisible map state x y =
    RectFov.isVisible state map x y

let depth map state x y =
    RectFov.translucencyDepth state map x y

let isExplored map state x y =
    RectFov.isExplored state map x y

let assertVisible map state x y message =
    assertTrue (isVisible map state x y) message

let assertHidden map state x y message =
    assertFalse (isVisible map state x y) message

let assertDepth expected map state x y message =
    assertEquals expected (depth map state x y) message

let assertExplored map state x y message =
    assertTrue (isExplored map state x y) message

let assertUnexplored map state x y message =
    assertFalse (isExplored map state x y) message

let testTranslucentTileVisibleAtCurrentDepthWithZeroBudget () =
    printfn "\n--- Test: Translucent tile visible at current depth with zero budget ---"

    let map, state, _ =
        compute [| ".@.G.." |] 0

    assertVisible map state 3 0 "Glass tile is visible even when no budget remains for cells behind it"
    assertDepth 0 map state 3 0 "Glass tile itself has clear depth before the budget is spent"
    assertExplored map state 3 0 "Glass tile seen directly is explored"
    assertHidden map state 4 0 "Tile behind glass is hidden when budget is zero"
    assertUnexplored map state 4 0 "Tile behind glass is not explored when only blocked by zero budget"

let testTranslucencyBudgetRevealsButDoesNotExploreBehindGlass () =
    printfn "\n--- Test: Budget reveals behind glass without exploring it ---"

    let map, state, _ =
        compute [| ".@.G.." |] 1

    assertVisible map state 3 0 "Glass tile is visible with budget one"
    assertDepth 0 map state 3 0 "Glass tile remains depth zero"
    assertVisible map state 4 0 "First tile behind glass is visible with budget one"
    assertDepth 1 map state 4 0 "First tile behind glass has translucency depth one"
    assertUnexplored map state 4 0 "Tile visible only through glass is not explored"
    assertVisible map state 5 0 "Sight continues behind a single translucent tile at depth one"
    assertDepth 1 map state 5 0 "Further clear tile behind the same glass keeps depth one"

let testBudgetExhaustionShowsLastGlassButStopsBehindIt () =
    printfn "\n--- Test: Exhausted budget shows last glass but stops behind it ---"

    let map, state, _ =
        compute [| ".@.G.G." |] 1

    assertVisible map state 3 0 "First glass tile is visible"
    assertDepth 0 map state 3 0 "First glass tile is seen directly"
    assertVisible map state 4 0 "Tile between two glass panes is visible through one pane"
    assertDepth 1 map state 4 0 "Tile between panes has depth one"
    assertVisible map state 5 0 "Second glass tile is visible even though passing it would exceed budget"
    assertDepth 1 map state 5 0 "Second glass tile is visible at the current depth"
    assertHidden map state 6 0 "Tile behind the second glass pane is hidden with budget one"
    assertUnexplored map state 5 0 "Second glass tile is not explored when it is only seen through earlier glass"

let testOpaqueWallFaceVisibleButBlocksBehindIt () =
    printfn "\n--- Test: Opaque wall face visible but blocks behind it ---"

    let map, state, _ =
        compute [| ".@.#.." |] 2

    assertVisible map state 3 0 "Opaque wall face is visible"
    assertDepth 0 map state 3 0 "Opaque wall face has clear depth"
    assertExplored map state 3 0 "Opaque wall face seen directly is explored"
    assertHidden map state 4 0 "Tile behind opaque wall is hidden"

let testZeroBudgetDoesNotLeakIntoSidePocketBehindGlass () =
    printfn "\n--- Test: Zero budget does not leak into side pocket behind glass ---"

    // This catches the over-permissive side of the old smoothing attempts:
    // a visible glass slit should not reveal adjacent side-pocket floor tiles
    // when the translucency budget is already exhausted.
    let rows =
        [| "########"
           "#@.G...#"
           "####...#"
           "########" |]

    let map, state, _ = compute rows 0

    assertVisible map state 3 1 "Glass slit is visible"
    assertDepth 0 map state 3 1 "Glass slit is directly visible"
    assertHidden map state 4 2 "Side pocket below the first behind-glass tile stays hidden"
    assertHidden map state 5 2 "Side pocket center stays hidden"
    assertHidden map state 6 2 "Far side pocket stays hidden"

let testShallowAngleGlassFaceRemainsVisible () =
    printfn "\n--- Test: Shallow-angle glass face remains visible ---"

    // This catches the over-strict side of the old smoothing attempts:
    // a glass face adjacent to clearly visible floor should not disappear just
    // because no convenient perimeter ray hits the cell center cleanly.
    let rows =
        [| "##########"
           "#@.......#"
           "######G###"
           "##########" |]

    let map, state, _ = compute rows 0

    assertVisible map state 6 1 "Floor above the glass face is visible"
    assertVisible map state 6 2 "Glass face next to visible floor is visible"
    assertDepth 0 map state 6 2 "Adjacent glass face is presented as clear depth"
    assertExplored map state 6 2 "Adjacent glass face is explored when presented as clear"

let testOpaqueFixtureBehindWallNextToDoorStaysHidden () =
    printfn "\n--- Test: Opaque fixture behind wall next to door stays hidden ---"

    // This catches the doorway-adjacent over-reveal case:
    // B is an opaque fixture on a floor tile. It is diagonally next to the open
    // doorway, but the wall segment above it should still occlude it.
    let rows =
        [| "########"
           "#@.....#"
           "####.###"
           "###B...#"
           "########" |]

    let map, state, _ = compute rows 0

    assertVisible map state 4 2 "Open doorway is visible"
    assertVisible map state 3 2 "Wall segment beside doorway is visible as a wall face"
    assertHidden map state 3 3 "Bookshelf behind that wall segment is not revealed by doorway face-fill"
    assertUnexplored map state 3 3 "Hidden bookshelf is not explored"

let testBracketedDoorwayVisibleByGeometry () =
    printfn "\n--- Test: Bracketed doorway is visible by geometry ---"

    let rows =
        [| "########"
           "#@.....#"
           "####.###"
           "########" |]

    let map, state, _ = compute rows 0

    assertVisible map state 4 2 "Doorway tile bracketed by walls is visible"
    assertDepth 0 map state 4 2 "Doorway tile is directly visible"
    assertExplored map state 4 2 "Doorway tile is explored when directly visible"

let testOpaqueFixtureInOpenSightIsVisibleAndBlocks () =
    printfn "\n--- Test: Opaque fixture in open sight is visible and blocks ---"

    let map, state, _ =
        compute [| ".@.B.." |] 0

    assertVisible map state 3 0 "Bookshelf fixture in direct line of sight is visible"
    assertDepth 0 map state 3 0 "Directly visible bookshelf has clear depth"
    assertExplored map state 3 0 "Directly visible bookshelf is explored"
    assertHidden map state 4 0 "Tile behind opaque bookshelf is hidden"

let testTwoGlassPanesSpendTwoBudget () =
    printfn "\n--- Test: Two glass panes spend two budget ---"

    let map, state, _ =
        compute [| ".@.G.G.." |] 2

    assertVisible map state 3 0 "First glass pane is visible"
    assertDepth 0 map state 3 0 "First glass pane is directly visible"
    assertVisible map state 5 0 "Second glass pane is visible"
    assertDepth 1 map state 5 0 "Second glass pane is visible through the first pane"
    assertVisible map state 6 0 "Tile behind two glass panes is visible with budget two"
    assertDepth 2 map state 6 0 "Tile behind two glass panes has depth two"
    assertUnexplored map state 6 0 "Tile visible only through two panes is not explored"

let testCornerTouchingWallsDoNotLeakSight () =
    printfn "\n--- Test: Corner-touching walls do not leak sight ---"

    let rows =
        [| "#####"
           "#@#.#"
           "##..#"
           "#...#"
           "#####" |]

    let map, state, _ = compute rows 0

    assertVisible map state 2 1 "East wall touching the corner is visible"
    assertVisible map state 1 2 "South wall touching the corner is visible"
    assertHidden map state 2 2 "Diagonal tile behind corner-touching walls is hidden"

let testTranslucentSeamDoesNotDoubleSpend () =
    printfn "\n--- Test: Translucent seam does not double spend ---"

    let rows =
        [| "######"
           "#@...#"
           "#.G..#"
           "#....#"
           "######" |]

    let map, state, _ = compute rows 1

    assertVisible map state 2 2 "Diagonal glass seam tile is visible"
    assertDepth 0 map state 2 2 "Diagonal glass seam tile is directly visible"
    assertVisible map state 3 3 "Tile behind diagonal glass is visible with budget one"
    assertDepth 1 map state 3 3 "Tile behind diagonal glass spends one budget, not two"

let testLargeWindowUsesSameVisibilityRules () =
    printfn "\n--- Test: Large window uses same visibility rules ---"

    let width = 101
    let height = 61
    let origin = (50, 30)
    let walls =
        [ for x in 0 .. width - 1 do
              yield (x, 0)
              yield (x, height - 1)
          for y in 1 .. height - 2 do
              yield (0, y)
              yield (width - 1, y)
          yield (60, 30) ]

    let glass = [ (55, 30) ]
    let rows = rowsFromPoints width height origin walls glass []
    let map, state, _ = compute rows 1

    assertVisible map state 55 30 "Glass in a large visibility window is visible"
    assertDepth 0 map state 55 30 "Large-window glass is directly visible"
    assertVisible map state 56 30 "Tile behind large-window glass is visible with budget one"
    assertDepth 1 map state 56 30 "Tile behind large-window glass has depth one"
    assertVisible map state 60 30 "Large-window wall face is visible"
    assertHidden map state 61 30 "Tile behind large-window wall is hidden"

let testVisibilityStateSizeMustMatchMap () =
    printfn "\n--- Test: VisibilityState size must match map ---"

    let map = makeMap [| ".@." |]
    let state = VisibilityState(map.Width + 1, map.Height)
    let origin = GridPos(1, 0)

    let threw =
        try
            RectFov.compute map state origin map.Width map.Height 0
            false
        with
        | :? ArgumentException -> true

    assertTrue threw "Mismatched VisibilityState dimensions are rejected"

let runTests () =
    printfn "\n========== FOV TESTS =========="
    testTranslucentTileVisibleAtCurrentDepthWithZeroBudget ()
    testTranslucencyBudgetRevealsButDoesNotExploreBehindGlass ()
    testBudgetExhaustionShowsLastGlassButStopsBehindIt ()
    testOpaqueWallFaceVisibleButBlocksBehindIt ()
    testZeroBudgetDoesNotLeakIntoSidePocketBehindGlass ()
    testShallowAngleGlassFaceRemainsVisible ()
    testOpaqueFixtureBehindWallNextToDoorStaysHidden ()
    testBracketedDoorwayVisibleByGeometry ()
    testOpaqueFixtureInOpenSightIsVisibleAndBlocks ()
    testTwoGlassPanesSpendTwoBudget ()
    testCornerTouchingWallsDoNotLeakSight ()
    testTranslucentSeamDoesNotDoubleSpend ()
    testLargeWindowUsesSameVisibilityRules ()
    testVisibilityStateSizeMustMatchMap ()
    printfn "\n========== ALL FOV TESTS PASSED =========="

runTests ()
