#r "bin/Debug/net8.0/AspectGameEngine.dll"

open System
open AspectGameEngine

let fail message =
    failwithf "ORACLE FAILED: %s" message

let tilesetName = "FovOracle_" + Guid.NewGuid().ToString("N")
let floorLoc = SpriteLoc(0, 0, 11)
let wallLoc = SpriteLoc(0, 0, 12)
let glassLoc = SpriteLoc(0, 0, 13)
let bookshelfFixtureId = 910002

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
        | [||] -> fail "map must contain an origin marked with '@'."
        | _ -> fail "map must contain exactly one origin marked with '@'."

let makeMap (rows: string[]) =
    if rows.Length = 0 then fail "map must have at least one row."
    let width = rows.[0].Length
    if width = 0 then fail "map rows must not be empty."

    for row in rows do
        if row.Length <> width then
            fail (sprintf "map rows must be rectangular. Expected %d but found %d in '%s'." width row.Length row)

    let height = rows.Length
    let tiles =
        rows
        |> Array.collect (fun row -> row.ToCharArray() |> Array.map tileForChar)

    let layerCells = Array.init (width * height) (fun _ -> LayerCell.Create())
    let map = TileMap(width, height, tiles, layerCells, wallLoc, tilesetName, "fov-oracle", MapType.Room)
    map.InitEffectiveOpacityCache()

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            if rows.[y].[x] = 'B' then
                map.SetFixture(x, y, bookshelfFixtureId)

    map

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

let scale = 16
let half = scale / 2

let sampleOffsets =
    [| for y in [ 2; 4; 6; 8; 10; 12; 14 ] do
           for x in [ 2; 4; 6; 8; 10; 12; 14 ] do
               yield (x, y) |]

let inline index width x y = y * width + x

let inline compareFractions n1 d1 n2 d2 =
    compare (int64 n1 * int64 d2) (int64 n2 * int64 d1)

let castSample (map: TileMap) (budget: int) (origin: GridPos) (targetX: int) (targetY: int) (sampleOffsetX: int, sampleOffsetY: int) =
    if targetX = origin.X && targetY = origin.Y then
        Some 0
    else
        let startX = origin.X * scale + half
        let startY = origin.Y * scale + half
        let endX = targetX * scale + sampleOffsetX
        let endY = targetY * scale + sampleOffsetY
        let dx = endX - startX
        let dy = endY - startY

        if dx = 0 && dy = 0 then
            Some 0
        else
            let stepX = if dx > 0 then 1 elif dx < 0 then -1 else 0
            let stepY = if dy > 0 then 1 elif dy < 0 then -1 else 0
            let absDx = abs dx
            let absDy = abs dy

            let mutable x = origin.X
            let mutable y = origin.Y
            let mutable cost = 0
            let mutable result = None
            let mutable blocked = false
            let mutable guard = 0
            let guardLimit = (map.Width + map.Height + 4) * 4

            while result.IsNone && not blocked && guard < guardLimit do
                guard <- guard + 1

                if x = targetX && y = targetY then
                    result <- Some cost
                else
                    if not (x = origin.X && y = origin.Y) then
                        match map.GetOpacity(x, y) with
                        | TileOpacity.Opaque ->
                            blocked <- true
                        | TileOpacity.Translucent ->
                            let nextCost = cost + 1
                            if nextCost > budget then
                                blocked <- true
                            else
                                cost <- nextCost
                        | TileOpacity.Transparent
                        | TileOpacity.Air -> ()
                        | _ ->
                            blocked <- true

                    if result.IsNone && not blocked then
                        let hasX = stepX <> 0
                        let hasY = stepY <> 0

                        if not hasX && not hasY then
                            blocked <- true
                        elif not hasX then
                            y <- y + stepY
                        elif not hasY then
                            x <- x + stepX
                        else
                            let boundaryX = if stepX > 0 then (x + 1) * scale else x * scale
                            let boundaryY = if stepY > 0 then (y + 1) * scale else y * scale
                            let txNum = abs (boundaryX - startX)
                            let tyNum = abs (boundaryY - startY)
                            let cmp = compareFractions txNum absDx tyNum absDy

                            if cmp < 0 then
                                x <- x + stepX
                            elif cmp > 0 then
                                y <- y + stepY
                            else
                                let sideX = x + stepX
                                let sideY = y + stepY

                                let sideBlocks sx sy =
                                    if sx < 0 || sx >= map.Width || sy < 0 || sy >= map.Height then
                                        true
                                    else
                                        match map.GetOpacity(sx, sy) with
                                        | TileOpacity.Transparent
                                        | TileOpacity.Air -> false
                                        | _ -> true

                                // A sampled ray that crosses exactly through a blocked grid corner
                                // is not a positive-measure visibility witness.
                                if sideBlocks sideX y || sideBlocks x sideY then
                                    blocked <- true
                                else
                                    x <- sideX
                                    y <- sideY

                        if x < 0 || x >= map.Width || y < 0 || y >= map.Height then
                            blocked <- true

            result

let oracleTileCost (map: TileMap) budget origin x y =
    let mutable best = Int32.MaxValue

    for sample in sampleOffsets do
        match castSample map budget origin x y sample with
        | Some cost when cost < best ->
            best <- cost
        | _ -> ()

    if best = Int32.MaxValue then None
    else Some best

type OracleCase =
    { Name: string
      Rows: string[]
      Budget: int }

let runCase (case: OracleCase) =
    let map = makeMap case.Rows
    let origin = findOrigin case.Rows
    let state = VisibilityState(map.Width, map.Height)
    RectFov.compute map state origin map.Width map.Height case.Budget

    let mutable hardFailures = []
    let mutable softMisses = []

    for y in 0 .. map.Height - 1 do
        for x in 0 .. map.Width - 1 do
            let idx = index map.Width x y
            let engineVisible = RectFov.isVisible state map x y
            let engineCost = if engineVisible then state.GetTranslucencyCost(idx) else Int32.MaxValue
            let oracleCost = oracleTileCost map case.Budget origin x y

            match oracleCost with
            | Some cost ->
                if not engineVisible then
                    hardFailures <- (x, y, sprintf "oracle visible at cost %d, engine hidden" cost) :: hardFailures
                elif engineCost > cost then
                    hardFailures <- (x, y, sprintf "oracle cost %d, engine cost %d" cost engineCost) :: hardFailures
            | None ->
                if engineVisible then
                    softMisses <- (x, y, engineCost) :: softMisses

    if hardFailures.Length > 0 then
        printfn "\nHard oracle failures in %s:" case.Name
        hardFailures
        |> List.rev
        |> List.truncate 20
        |> List.iter (fun (x, y, msg) -> printfn "  (%d,%d): %s" x y msg)
        fail (sprintf "%s had %d hard oracle failures." case.Name hardFailures.Length)

    if softMisses.Length > 0 then
        printfn "SOFT: %s has %d engine-visible tiles missed by finite sampling." case.Name softMisses.Length
        softMisses
        |> List.rev
        |> List.truncate 10
        |> List.iter (fun (x, y, cost) -> printfn "  soft (%d,%d): engine cost %d" x y cost)
    else
        printfn "PASSED: %s (no soft misses)" case.Name

let randomRows seed width height budget =
    let rng = Random(seed)
    let origin = (width / 2, height / 2)

    Array.init height (fun y ->
        Array.init width (fun x ->
            if x = fst origin && y = snd origin then '@'
            elif x = 0 || y = 0 || x = width - 1 || y = height - 1 then '#'
            else
                let roll = rng.NextDouble()
                if roll < 0.13 then '#'
                elif roll < 0.22 then 'G'
                else '.')
        |> String)

let characterizationCases =
    [ { Name = "zero-budget glass"
        Rows = [| ".@.G.." |]
        Budget = 0 }
      { Name = "glass budget one"
        Rows = [| ".@.G.." |]
        Budget = 1 }
      { Name = "two panes budget one"
        Rows = [| ".@.G.G." |]
        Budget = 1 }
      { Name = "side pocket behind glass"
        Rows =
            [| "########"
               "#@.G...#"
               "####...#"
               "########" |]
        Budget = 0 }
      { Name = "doorway bookshelf"
        Rows =
            [| "########"
               "#@.....#"
               "####.###"
               "###B...#"
               "########" |]
        Budget = 0 }
      { Name = "corner-touch walls"
        Rows =
            [| "#####"
               "#@#.#"
               "##..#"
               "#...#"
               "#####" |]
        Budget = 0 }
      { Name = "diagonal glass seam"
        Rows =
            [| "######"
               "#@...#"
               "#.G..#"
               "#....#"
               "######" |]
        Budget = 1 } ]

let largeRows =
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

    rowsFromPoints width height origin walls [ (55, 30) ] []

let randomCases =
    [ for i in 0 .. 49 do
          let budget =
              match i % 4 with
              | 0 -> 0
              | 1 -> 1
              | 2 -> 2
              | _ -> 255

          let width = 17 + ((i * 7) % 17)
          let height = 13 + ((i * 5) % 13)

          { Name = sprintf "random-%02d-budget-%d" i budget
            Rows = randomRows (1000 + i) width height budget
            Budget = budget } ]

let allCases =
    characterizationCases
    @ [ { Name = "large-window-budget-one"
          Rows = largeRows
          Budget = 1 } ]
    @ randomCases

printfn "\n========== FOV ORACLE =========="
printfn "Running %d oracle cases with %d samples per tile." allCases.Length sampleOffsets.Length

allCases |> List.iter runCase

printfn "\n========== FOV ORACLE HARD CHECKS PASSED =========="

// ---------------------------------------------------------------------------
// Window-clip differential check.
//
// All sampled-oracle cases above run with the window equal to the full map,
// so the rectangular clipping path in RectFov.compute (per-octant depth and
// minor clamps) is not exercised by them. The window rectangle is convex and
// contains the origin, so every sightline from the origin to an in-window
// tile stays inside the window. Therefore clipped-window visibility must
// equal full-map visibility intersected with the window, with identical
// costs, and nothing outside the window may be stamped. Any mismatch is a
// clipping bug.
// ---------------------------------------------------------------------------

let clipRandomRows seed width height (ox: int) (oy: int) =
    let rng = Random(seed)

    Array.init height (fun y ->
        Array.init width (fun x ->
            if x = ox && y = oy then '@'
            elif x = 0 || y = 0 || x = width - 1 || y = height - 1 then '#'
            else
                let roll = rng.NextDouble()
                if roll < 0.13 then '#'
                elif roll < 0.22 then 'G'
                else '.')
        |> String)

let mutable clipComparisons = 0
let mutable clipMismatches = 0

for seed in 0 .. 19 do
    let width = 41 + (seed % 5)
    let height = 31 + (seed % 7)
    let ox = 5 + (seed * 3) % (width - 10)
    let oy = 5 + (seed * 5) % (height - 10)
    let budget = seed % 4
    let halfW = 4 + (seed % 9)
    let halfH = 3 + (seed % 7)

    let rows = clipRandomRows (5000 + seed) width height ox oy
    let map = makeMap rows
    let origin = findOrigin rows

    let fullState = VisibilityState(map.Width, map.Height)
    RectFov.compute map fullState origin map.Width map.Height budget

    let clipState = VisibilityState(map.Width, map.Height)
    RectFov.compute map clipState origin halfW halfH budget

    let minX = max 0 (ox - halfW)
    let maxX = min (width - 1) (ox + halfW)
    let minY = max 0 (oy - halfH)
    let maxY = min (height - 1) (oy + halfH)

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let idx = index width x y
            let inWindow = x >= minX && x <= maxX && y >= minY && y <= maxY
            let clipVisible = clipState.IsVisible(idx)
            clipComparisons <- clipComparisons + 1

            if not inWindow then
                if clipVisible then
                    clipMismatches <- clipMismatches + 1
                    printfn "CLIP FAIL seed=%d: tile (%d,%d) outside window is stamped" seed x y
            else
                if clipVisible <> fullState.IsVisible(idx) then
                    clipMismatches <- clipMismatches + 1
                    printfn
                        "CLIP FAIL seed=%d: tile (%d,%d) visibility clip=%b full=%b (budget=%d origin=%d,%d half=%dx%d)"
                        seed x y clipVisible (fullState.IsVisible(idx)) budget ox oy halfW halfH
                elif clipVisible && clipState.GetTranslucencyCost(idx) <> fullState.GetTranslucencyCost(idx) then
                    clipMismatches <- clipMismatches + 1
                    printfn
                        "CLIP FAIL seed=%d: tile (%d,%d) cost clip=%d full=%d"
                        seed x y (clipState.GetTranslucencyCost(idx)) (fullState.GetTranslucencyCost(idx))

if clipMismatches > 0 then
    fail (sprintf "window-clip differential check had %d mismatches." clipMismatches)

printfn "\nPASSED: window-clip differential check (%d tile comparisons across 20 seeded maps)." clipComparisons
printfn "\n========== FOV ORACLE WINDOW-CLIP CHECK PASSED =========="
