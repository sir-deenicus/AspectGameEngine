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

let isDisclosed map state x y =
    RectFov.isDisclosed state map x y

let isPresentedVisible map state x y =
    RectFov.isPresentedVisible state map x y

let depth map state x y =
    RectFov.translucencyDepth state map x y

let presentedDepth map state x y =
    RectFov.presentedDepth state map x y

let volumeCost (map: TileMap) (state: VisibilityState) x y =
    state.GetTranslucencyCost(y * map.Width + x)

let isExplored map state x y =
    RectFov.isExplored state map x y

let assertVisible map state x y message =
    assertTrue (isVisible map state x y) message

let assertHidden map state x y message =
    assertFalse (isVisible map state x y) message

let assertDisclosed map state x y message =
    assertTrue (isDisclosed map state x y) message

let assertNotDisclosed map state x y message =
    assertFalse (isDisclosed map state x y) message

let assertPresentedVisible map state x y message =
    assertTrue (isPresentedVisible map state x y) message

let assertNotPresentedVisible map state x y message =
    assertFalse (isPresentedVisible map state x y) message

let assertDepth expected map state x y message =
    assertEquals expected (depth map state x y) message

let assertPresentedDepth expected map state x y message =
    assertEquals expected (presentedDepth map state x y) message

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
    assertPresentedVisible map state 4 0 "First tile behind glass is presented as visible"
    assertPresentedDepth 1 map state 4 0 "Presented depth preserves through-glass cost for frontend dimming"
    assertUnexplored map state 4 0 "Tile visible only through glass is not explored"
    assertVisible map state 5 0 "Sight continues behind a single translucent tile at depth one"
    assertDepth 1 map state 5 0 "Further clear tile behind the same glass keeps depth one"

let testExploredOpaqueFixtureThroughGlassKeepsDepth () =
    printfn "\n--- Test: Explored opaque fixture through glass keeps depth ---"

    // Two computes are required for this regression. The first step explores
    // the room from inside at cost 0. The second step views the same explored
    // surfaces through a glass pane, where presented depth must report the
    // current volume cost rather than flattening explored opaque cells to 0.
    let rows =
        [| "########"
           ".#.....#"
           ".G...B.#"
           ".#.....#"
           "########" |]

    let map = makeMap rows
    let state = VisibilityState(map.Width, map.Height)
    let insideOrigin = GridPos(4, 2)
    let outsideOrigin = GridPos(0, 2)

    assertEquals TileOpacity.Opaque (map.GetOpacity(5, 2)) "Bookshelf fixture contributes effective opacity"

    RectFov.compute map state insideOrigin map.Width map.Height 1
    assertVisible map state 5 2 "Bookshelf is visible before moving outside"
    assertDepth 0 map state 5 2 "Bookshelf is directly visible before moving outside"
    assertExplored map state 5 2 "Bookshelf is explored by direct sight"
    assertExplored map state 3 2 "Floor behind the future glass view is explored by direct sight"

    RectFov.compute map state outsideOrigin map.Width map.Height 1

    assertVisible map state 1 2 "Glass pane is visible from outside"
    assertDepth 0 map state 1 2 "Directly seen glass pane stays depth zero"
    assertPresentedDepth 0 map state 1 2 "Directly seen glass pane presents as clear"

    assertVisible map state 3 2 "Previously explored floor is visible through glass"
    assertEquals 1 (volumeCost map state 3 2) "Previously explored floor has volume cost one through glass"
    assertDepth 1 map state 3 2 "Previously explored floor reports through-glass depth"
    assertPresentedDepth 1 map state 3 2 "Previously explored floor presents through-glass depth"

    assertVisible map state 5 2 "Previously explored bookshelf is visible through glass"
    assertEquals 1 (volumeCost map state 5 2) "Previously explored bookshelf has volume cost one through glass"
    assertDepth 1 map state 5 2 "Previously explored opaque fixture reports through-glass depth"
    assertPresentedDepth 1 map state 5 2 "Previously explored opaque fixture presents through-glass depth"
    assertExplored map state 5 2 "Bookshelf remains explored after the through-glass view"

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
    assertNotDisclosed map state 3 3 "Bookshelf is not disclosed by the doorway-adjacent surface rule"
    assertNotPresentedVisible map state 3 3 "Bookshelf is not presented when it is only diagonal to the doorway"
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
    assertNotDisclosed map state 2 2 "Open diagonal floor behind corner-touching walls is not disclosed"
    assertNotPresentedVisible map state 2 2 "Open diagonal floor behind corner-touching walls is not presented"
    assertUnexplored map state 2 2 "Open diagonal floor behind corner-touching walls is not explored"

let testRoomCornersAreDisclosedForPresentation () =
    printfn "\n--- Test: Room corners are disclosed for presentation ---"

    let rows =
        [| "#####"
           "#...#"
           "#.@.#"
           "#...#"
           "#####" |]

    let map, state, _ = compute rows 0

    for x, y in [ (0, 0); (4, 0); (0, 4); (4, 4) ] do
        assertHidden map state x y (sprintf "Room corner (%d,%d) stays hidden from volume FOV" x y)
        assertDisclosed map state x y (sprintf "Room corner (%d,%d) is disclosed as a surface" x y)
        assertPresentedVisible map state x y (sprintf "Room corner (%d,%d) is presented visible" x y)
        assertPresentedDepth 0 map state x y (sprintf "Room corner (%d,%d) has direct presented depth" x y)
        assertExplored map state x y (sprintf "Room corner (%d,%d) is explored by disclosure" x y)

    let walls =
        [ for x in 0 .. 4 do
              yield (x, 0)
              yield (x, 4)
          for y in 1 .. 3 do
              yield (0, y)
              yield (4, y) ]

    for origin, corner in [ (1, 1), (0, 0); (3, 1), (4, 0); (1, 3), (0, 4); (3, 3), (4, 4) ] do
        let rows = rowsFromPoints 5 5 origin walls [] []
        let map, state, _ = compute rows 0
        let cx, cy = corner
        assertHidden map state cx cy (sprintf "Adjacent room corner (%d,%d) stays hidden from volume FOV" cx cy)
        assertDisclosed map state cx cy (sprintf "Adjacent room corner (%d,%d) is disclosed" cx cy)
        assertPresentedVisible map state cx cy (sprintf "Adjacent room corner (%d,%d) is presented visible" cx cy)
        assertExplored map state cx cy (sprintf "Adjacent room corner (%d,%d) is explored by disclosure" cx cy)

let testGlassDoesNotDiscloseWallBehindIt () =
    printfn "\n--- Test: Glass does not disclose wall behind it ---"

    let map, state, _ = compute [| "@G#" |] 0

    assertVisible map state 1 0 "Glass is directly visible at zero budget"
    assertHidden map state 2 0 "Wall behind glass is not volume-visible when budget is zero"
    assertNotDisclosed map state 2 0 "Glass is translucent, not a cost-zero transparent discloser"
    assertNotPresentedVisible map state 2 0 "Wall behind glass is not presented by disclosure"
    assertUnexplored map state 2 0 "Wall behind glass is not explored by disclosure"

let testWallBehindWallIsNotDisclosed () =
    printfn "\n--- Test: Wall behind wall is not disclosed ---"

    let map, state, _ = compute [| "@##" |] 0

    assertVisible map state 1 0 "Near wall is directly visible"
    assertHidden map state 2 0 "Far wall behind near wall is not volume-visible"
    assertNotDisclosed map state 2 0 "Far wall is not disclosed through another wall"
    assertNotPresentedVisible map state 2 0 "Far wall behind near wall is not presented"
    assertUnexplored map state 2 0 "Far wall behind near wall is not explored"

let testNearWindowWallSurfacePresentationOverridesGlassSliver () =
    printfn "\n--- Test: Near-window wall surface presentation overrides glass sliver ---"

    let rows =
        [| "....."
           "..##."
           "..G@."
           "....." |]

    let map, state, _ = compute rows 1

    assertVisible map state 2 1 "Near wall is volume-visible through the adjacent window sliver"
    assertDepth 1 map state 2 1 "Volume depth preserves the through-glass sliver"
    assertNotDisclosed map state 2 1 "Volume-visible surface presentation is not reported as disclosure"
    assertPresentedVisible map state 2 1 "Near wall is still presented visible"
    assertPresentedDepth 0 map state 2 1 "Direct surface presentation wins over through-glass depth"
    assertExplored map state 2 1 "Direct surface presentation explores the near wall"
    assertHidden map state 1 0 "Surface presentation does not open visibility behind the wall"
    assertNotPresentedVisible map state 1 0 "Surface presentation does not present cells behind the wall"

let testNearWindowWallDisclosureIsMonotonicAtZeroBudget () =
    printfn "\n--- Test: Near-window wall disclosure is monotonic at zero budget ---"

    let rows =
        [| "....."
           "..##."
           "..G@."
           "....." |]

    let map, state, _ = compute rows 0

    assertHidden map state 2 1 "Near wall is hidden from volume FOV when glass cannot pass budget"
    assertDepth -1 map state 2 1 "Hidden near wall has no volume depth"
    assertDisclosed map state 2 1 "Origin-adjacent surface disclosure presents the near wall"
    assertPresentedVisible map state 2 1 "Disclosed near wall is presented visible at zero budget"
    assertPresentedDepth 0 map state 2 1 "Zero-budget disclosure presents the near wall clearly"
    assertExplored map state 2 1 "Zero-budget disclosure explores the near wall"

let testOpenNeighborWallStillPresentsDirectly () =
    printfn "\n--- Test: Open-neighbor wall still presents directly ---"

    let rows =
        [| "....."
           "..#.."
           "..G@."
           "....." |]

    let map, state, _ = compute rows 1

    assertVisible map state 2 1 "Wall with an open clear neighbor is directly volume-visible"
    assertDepth 0 map state 2 1 "Directly visible wall remains volume depth zero"
    assertNotDisclosed map state 2 1 "Direct volume visibility is not disclosure"
    assertPresentedDepth 0 map state 2 1 "Directly visible wall presents clearly"
    assertExplored map state 2 1 "Directly visible wall is explored"

let testEndOfWallSliverDoesNotDiscloseFixtureBehindIt () =
    printfn "\n--- Test: End-of-wall sliver does not disclose fixture behind it ---"

    let rows =
        [| "@....."
           "#####."
           ".....B" |]

    let map, state, _ = compute rows 0

    assertEquals TileOpacity.Opaque (map.GetOpacity(5, 2)) "Statue fixture contributes effective opacity"
    assertVisible map state 5 1 "Open floor past the wall end remains volume-visible"
    assertHidden map state 5 2 "Statue behind the wall-end shadow is not volume-visible"
    assertNotDisclosed map state 5 2 "Wall-end floor sliver does not disclose the statue"
    assertNotPresentedVisible map state 5 2 "Statue is not presented by boundary-unsupported disclosure"
    assertPresentedDepth -1 map state 5 2 "Hidden statue has no presented depth"
    assertUnexplored map state 5 2 "Hidden statue is not explored"

let testDisclosureSupportRayCornerTieBreak () =
    printfn "\n--- Test: Disclosure support ray corner tie-break ---"

    let blockedRows =
        [| "##."
           "#.#"
           ".#@" |]

    let blockedMap, blockedState, _ = compute blockedRows 0

    assertHidden blockedMap blockedState 0 0 "Sealed corner candidate stays hidden from volume FOV"
    assertNotDisclosed blockedMap blockedState 0 0 "Support ray is blocked when both pinching tiles are opaque"
    assertNotPresentedVisible blockedMap blockedState 0 0 "Pinched support ray does not present the corner"
    assertUnexplored blockedMap blockedState 0 0 "Pinched support ray does not explore the corner"

    let openRows =
        [| "##."
           "#.."
           ".#@" |]

    let openMap, openState, _ = compute openRows 0

    assertVisible openMap openState 1 1 "Diagonal discloser floor is visible when one pinching tile is open"
    assertHidden openMap openState 0 0 "Corner candidate still stays hidden from volume FOV"
    assertDisclosed openMap openState 0 0 "Support ray passes when one pinching tile is open"
    assertPresentedVisible openMap openState 0 0 "Corner is presented by the supported diagonal disclosure"
    assertExplored openMap openState 0 0 "Supported disclosure explores the corner"

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
    testExploredOpaqueFixtureThroughGlassKeepsDepth ()
    testBudgetExhaustionShowsLastGlassButStopsBehindIt ()
    testOpaqueWallFaceVisibleButBlocksBehindIt ()
    testZeroBudgetDoesNotLeakIntoSidePocketBehindGlass ()
    testShallowAngleGlassFaceRemainsVisible ()
    testOpaqueFixtureBehindWallNextToDoorStaysHidden ()
    testBracketedDoorwayVisibleByGeometry ()
    testOpaqueFixtureInOpenSightIsVisibleAndBlocks ()
    testTwoGlassPanesSpendTwoBudget ()
    testCornerTouchingWallsDoNotLeakSight ()
    testRoomCornersAreDisclosedForPresentation ()
    testGlassDoesNotDiscloseWallBehindIt ()
    testWallBehindWallIsNotDisclosed ()
    testNearWindowWallSurfacePresentationOverridesGlassSliver ()
    testNearWindowWallDisclosureIsMonotonicAtZeroBudget ()
    testOpenNeighborWallStillPresentsDirectly ()
    testEndOfWallSliverDoesNotDiscloseFixtureBehindIt ()
    testDisclosureSupportRayCornerTieBreak ()
    testTranslucentSeamDoesNotDoubleSpend ()
    testLargeWindowUsesSameVisibilityRules ()
    testVisibilityStateSizeMustMatchMap ()
    printfn "\n========== ALL FOV TESTS PASSED =========="

runTests ()
