#r "bin/Debug/net8.0/AspectGameEngine.dll"
open AspectGameEngine

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let makeExpectedEmptyTile (mapInstance: EditorTileMap) : Tile =
    { SpriteLoc = mapInstance.VoidSpriteLoc
      Health = 0
      DestroyedSpriteLoc = None }

let DefaultVoidSprite = SpriteLoc(0, 0, 0)
let ModifiedSprite = SpriteLoc(1, 1, 1)

// --- Test Functions ---
let testResizeWidthOnly () =
    printfn "\n--- Test: Resize Width Only (2x2 -> 3x2) ---"
    let initialVoidSprite = DefaultVoidSprite // Assumes this SpriteLoc variant exists
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, Map.empty)

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 100
          DestroyedSpriteLoc = None } // Assumes ModifiedSprite exists

    let mapWithModifiedTile = initialMap.UpdateTile(0, 0, modifiedTileData)
    let resizedMap = mapWithModifiedTile.Resize(3, 2)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap

    assertEquals 3 resizedMap.Width "Width"
    assertEquals 2 resizedMap.Height "Height"
    assertEquals modifiedTileData (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 0)) "New Tile (2,0)"
    printfn "--- TestResizeWidthOnly: PASSED ---"

let testResizeHeightOnly () =
    printfn "\n--- Test: Resize Height Only (2x2 -> 2x3) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, Map.empty)

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 50
          DestroyedSpriteLoc = None }

    let mapWithModifiedTile = initialMap.UpdateTile(1, 1, modifiedTileData)
    let resizedMap = mapWithModifiedTile.Resize(2, 3)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap

    assertEquals 2 resizedMap.Width "Width"
    assertEquals 3 resizedMap.Height "Height"
    assertEquals modifiedTileData (resizedMap.GetTile(1, 1)) "Modified Tile (1,1)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(0, 2)) "New Tile (0,2)"
    printfn "--- TestResizeHeightOnly: PASSED ---"

let testResizeWidthAndHeight () =
    printfn "\n--- Test: Resize Both (2x2 -> 3x1) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, Map.empty)

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          DestroyedSpriteLoc = None }

    let mapWithModifications = initialMap.UpdateTile(0, 0, modifiedTile00)
    let resizedMap = mapWithModifications.Resize(3, 1)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap

    assertEquals 3 resizedMap.Width "Width"
    assertEquals 1 resizedMap.Height "Height"
    assertEquals modifiedTile00 (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 0)) "New Tile (2,0)"
    // Check out-of-bounds access
    try
        let _ = resizedMap.GetTile(0, 1)
        failwith "Accessed out-of-bounds tile"
    with _ ->
        printfn "PASSED: Out-of-bounds access failed as expected."

    printfn "--- TestResizeWidthAndHeight: PASSED ---"

let testResizeWidthAndHeight2 () =
    printfn "\n--- Test: Resize Both (2x2 -> 3x4) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, Map.empty)

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          DestroyedSpriteLoc = None }

    let mapWithModifications = initialMap.UpdateTile(0, 0, modifiedTile00)
    let resizedMap = mapWithModifications.Resize(3, 4)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap
    assertEquals 3 resizedMap.Width "Width"
    assertEquals 4 resizedMap.Height "Height"
    assertEquals modifiedTile00 (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 3)) "New Tile (2,3)"
    // Check out-of-bounds access
    try
        let _ = resizedMap.GetTile(0, 4)
        failwith "Accessed out-of-bounds tile"
    with _ ->
        printfn "PASSED: Out-of-bounds access failed as expected."

    printfn "--- TestResizeWidthAndHeight2: PASSED ---"

testResizeWidthOnly ()
testResizeHeightOnly ()
testResizeWidthAndHeight ()
testResizeWidthAndHeight2 ()
