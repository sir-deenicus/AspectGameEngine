#r "bin/Debug/net8.0/AspectGameEngine.dll"

open AspectGameEngine

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let rect x y w h : RectI =
    { X = x
      Y = y
      W = w
      H = h }

let sortRects (rects: seq<RectI>) =
    rects
    |> Seq.toList
    |> List.sortBy (fun r -> r.X, r.Y, r.W, r.H)

let chunkRects (manager: ChunkOcclusionManager) cx cy =
    manager.GetChunkRects(cx, cy) |> sortRects

let chunkDoorRects (manager: ChunkOcclusionManager) cx cy =
    manager.GetChunkDoorRects(cx, cy) |> sortRects

let rectsInView (manager: ChunkOcclusionManager) x0 y0 x1 y1 =
    let rects = ResizeArray<RectI>()
    manager.ForEachRectInView(x0, y0, x1, y1, fun r -> rects.Add r)
    rects |> sortRects

let setRun (manager: ChunkOcclusionManager) y x0 x1 =
    for x = x0 to x1 do
        manager.SetOpaque(x, y, true)

let setColumn (manager: ChunkOcclusionManager) x y0 y1 =
    for y = y0 to y1 do
        manager.SetOpaque(x, y, true)

let setBlock (manager: ChunkOcclusionManager) x0 y0 x1 y1 =
    for y = y0 to y1 do
        for x = x0 to x1 do
            manager.SetOpaque(x, y, true)

let testEmptyMapEmitsNoRects () =
    printfn "\n--- Test: Empty Map Emits No Rects ---"
    let manager = ChunkOcclusionManager(4, 4, 4)

    assertEquals 0 (manager.RebuildDirty()) "No dirty chunks before any opacity changes"
    assertEquals [] (chunkRects manager 0 0) "Empty chunk has no ordinary rects"
    assertEquals [] (chunkDoorRects manager 0 0) "Empty chunk has no door rects"
    assertEquals [] (rectsInView manager 0 0 4 4) "Empty view query emits no rects"

let testSingleCellAndRemoval () =
    printfn "\n--- Test: Single Cell And Removal ---"
    let manager = ChunkOcclusionManager(4, 4, 4)

    manager.SetOpaque(1, 2, true)
    assertEquals 1 (manager.RebuildDirty()) "Single opaque cell dirties one chunk"
    assertEquals [ rect 1 2 1 1 ] (chunkRects manager 0 0) "Single opaque cell emits 1x1 rect"

    manager.SetOpaque(1, 2, false)
    assertEquals 1 (manager.RebuildDirty()) "Clearing an opaque cell dirties one chunk"
    assertEquals [] (chunkRects manager 0 0) "Cleared cell removes stale rect"

let testRunsMerge () =
    printfn "\n--- Test: Horizontal And Vertical Runs Merge ---"
    let manager = ChunkOcclusionManager(8, 8, 8)

    setRun manager 1 1 3
    setColumn manager 5 0 2

    assertEquals 1 (manager.RebuildDirty()) "Runs in one chunk rebuild once"
    assertEquals
        [ rect 1 1 3 1
          rect 5 0 1 3 ]
        (chunkRects manager 0 0)
        "Contiguous horizontal and vertical runs merge into long rectangles"

let testRectangularBlockMerges () =
    printfn "\n--- Test: Rectangular Block Merges ---"
    let manager = ChunkOcclusionManager(8, 8, 8)

    setBlock manager 2 2 4 3

    assertEquals 1 (manager.RebuildDirty()) "Rectangular block dirties one chunk"
    assertEquals [ rect 2 2 3 2 ] (chunkRects manager 0 0) "Solid block emits one rectangle"

let testSeparatedBlocksStaySeparate () =
    printfn "\n--- Test: Separated Blocks Stay Separate ---"
    let manager = ChunkOcclusionManager(6, 4, 6)

    manager.SetOpaque(0, 0, true)
    manager.SetOpaque(2, 0, true)
    setBlock manager 4 1 5 2

    assertEquals 1 (manager.RebuildDirty()) "Separated shapes in one chunk rebuild once"
    assertEquals
        [ rect 0 0 1 1
          rect 2 0 1 1
          rect 4 1 2 2 ]
        (chunkRects manager 0 0)
        "Transparent gaps prevent rectangle fusion"

let testChunkBoundariesDoNotMerge () =
    printfn "\n--- Test: Chunk Boundaries Do Not Merge ---"
    let manager = ChunkOcclusionManager(8, 2, 4)

    setRun manager 0 2 5

    assertEquals 2 (manager.RebuildDirty()) "Run crossing a chunk boundary dirties two chunks"
    assertEquals [ rect 2 0 2 1 ] (chunkRects manager 0 0) "Left chunk owns its side of the run"
    assertEquals [ rect 4 0 2 1 ] (chunkRects manager 1 0) "Right chunk owns its side of the run"
    assertEquals
        [ rect 2 0 2 1
          rect 4 0 2 1 ]
        (rectsInView manager 0 0 8 2)
        "View query streams both chunk-local rectangles without merging them"

let testDoorsStaySeparateAndToggle () =
    printfn "\n--- Test: Doors Stay Separate And Toggle ---"
    let manager = ChunkOcclusionManager(5, 3, 5)

    manager.SetOpaque(0, 1, true)
    manager.SetOpaque(1, 1, true)
    manager.SetDoorCellState(2, 1, TileOpacity.Opaque)
    manager.SetOpaque(3, 1, true)

    assertEquals 1 (manager.RebuildDirty()) "Wall and door changes in one chunk rebuild once"
    assertEquals
        [ rect 0 1 2 1
          rect 3 1 1 1 ]
        (chunkRects manager 0 0)
        "Closed door is excluded from ordinary merged geometry"
    assertEquals [ rect 2 1 1 1 ] (chunkDoorRects manager 0 0) "Closed door emits a dedicated 1x1 rect"

    manager.SetDoorCellState(2, 1, TileOpacity.Transparent)
    assertEquals 1 (manager.RebuildDirty()) "Opening a closed door dirties its chunk"
    assertEquals [] (chunkDoorRects manager 0 0) "Open door emits no door rect"
    assertEquals
        [ rect 0 1 2 1
          rect 3 1 1 1 ]
        (chunkRects manager 0 0)
        "Opening the door does not fuse ordinary geometry across the door cell"

let registerFixture id opacity =
    EntityRegistry.SpriteProps.[id] <-
        { Sprite = SheetCell(SpriteSheetCell(0, 0, 0))
          SpriteType =
            SpriteType.Fixture
                { BlocksMovement = false
                  Interactable = false
                  Moveable = 0
                  DescKey = ""
                  TileOpacity = opacity }
          RenderLayer = 0 }

let registerActor id opacity =
    EntityRegistry.SpriteProps.[id] <-
        { Sprite = SheetCell(SpriteSheetCell(0, 0, 0))
          SpriteType =
            SpriteType.Actor
                { TileOpacity = opacity
                  DescKey = ""
                  NpcFrames = None }
          RenderLayer = 0 }

let testSetFromTileAndLayerRespectsEffectiveOpacity () =
    printfn "\n--- Test: SetFromTileAndLayer Respects Effective Opacity ---"
    let fixtureId = 910001
    let actorId = 910002
    registerFixture fixtureId TileOpacity.Opaque
    registerActor actorId TileOpacity.Transparent

    let manager = ChunkOcclusionManager(4, 4, 4)
    let emptyCell = LayerCell.Create()
    let fixtureCell = LayerCell.Create()
    fixtureCell.FixtureId <- Some fixtureId
    let actorPriorityCell = LayerCell.Create()
    actorPriorityCell.FixtureId <- Some fixtureId

    manager.SetFromTileAndLayer(0, 0, TileOpacity.Opaque, emptyCell)
    manager.SetFromTileAndLayer(1, 0, TileOpacity.Transparent, fixtureCell)
    manager.SetFromTileAndLayer(2, 0, TileOpacity.Transparent, actorPriorityCell)

    assertEquals 1 (manager.RebuildDirty()) "Base and fixture opacity changes dirty one chunk"
    assertEquals [ rect 0 0 3 1 ] (chunkRects manager 0 0) "Base tile and opaque fixture both contribute opacity"

    actorPriorityCell.ActorId <- Some actorId
    manager.SetFromTileAndLayer(2, 0, TileOpacity.Transparent, actorPriorityCell)

    assertEquals 1 (manager.RebuildDirty()) "Actor opacity change dirties one chunk"
    assertEquals [ rect 0 0 2 1 ] (chunkRects manager 0 0) "Actor opacity takes precedence over fixture opacity"

let testClearAllClearsDataAndOutput () =
    printfn "\n--- Test: ClearAll Clears Data And Output ---"
    let manager = ChunkOcclusionManager(4, 4, 4)

    manager.SetOpaque(0, 0, true)
    manager.SetDoorCellState(1, 0, TileOpacity.Opaque)
    assertEquals 1 (manager.RebuildDirty()) "Initial mixed occluders rebuild one chunk"
    assertEquals [ rect 0 0 1 1 ] (chunkRects manager 0 0) "Ordinary rect exists before clear"
    assertEquals [ rect 1 0 1 1 ] (chunkDoorRects manager 0 0) "Door rect exists before clear"

    manager.ClearAll()

    assertEquals 0 (manager.RebuildDirty()) "ClearAll leaves no dirty chunks"
    assertEquals [] (chunkRects manager 0 0) "ClearAll removes ordinary rects"
    assertEquals [] (chunkDoorRects manager 0 0) "ClearAll removes door rects"
    assertEquals [] (rectsInView manager 0 0 4 4) "ClearAll removes view output"

let testForEachRectInViewFiltersByExclusiveFarEdge () =
    printfn "\n--- Test: ForEachRectInView Filters By Exclusive Far Edge ---"
    let manager = ChunkOcclusionManager(8, 8, 4)

    setRun manager 2 1 3
    manager.SetOpaque(7, 7, true)

    assertEquals 2 (manager.RebuildDirty()) "View-filter setup dirties each touched chunk"
    assertEquals [] (rectsInView manager 0 0 1 4) "Rect starting on the far x edge is outside the view"
    assertEquals [ rect 1 2 3 1 ] (rectsInView manager 3 0 4 4) "Partial overlap is included"
    assertEquals [ rect 7 7 1 1 ] (rectsInView manager 7 7 8 8) "Single-cell view at bottom-right includes matching rect"

let tests =
    [ testEmptyMapEmitsNoRects
      testSingleCellAndRemoval
      testRunsMerge
      testRectangularBlockMerges
      testSeparatedBlocksStaySeparate
      testChunkBoundariesDoNotMerge
      testDoorsStaySeparateAndToggle
      testSetFromTileAndLayerRespectsEffectiveOpacity
      testClearAllClearsDataAndOutput
      testForEachRectInViewFiltersByExclusiveFarEdge ]

for test in tests do
    test ()

printfn "\nAll occluder tests passed."
