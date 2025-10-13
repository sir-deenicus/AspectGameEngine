#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"
open AspectGameEngine 
open FSharpx.Collections
open System.Reflection
open System
open System.Diagnostics

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let assertTrue cond message =
    if not cond then failwithf "ASSERTION FAILED: %s" message
    else printfn "PASSED: %s" message

let makeExpectedEmptyTile (mapInstance: EditorTileMap) : Tile =
    { SpriteLoc = mapInstance.VoidSpriteLoc
      Health = 0
      IsOccupied = false }

let DefaultVoidSprite = SpriteLoc(0, 0, 0)
let ModifiedSprite = SpriteLoc(1, 1, 1)

// --- Test Functions ---
let testResizeWidthOnly () =
    printfn "\n--- Test: Resize Width Only (2x2 -> 3x2) ---"
    let initialVoidSprite = DefaultVoidSprite // Assumes this SpriteLoc variant exists
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, TilePropertiesImmutableReference.New(""))

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 100
          IsOccupied = false } // Assumes ModifiedSprite exists

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
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite,TilePropertiesImmutableReference.New(""))

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 50
          IsOccupied = false }

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
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, TilePropertiesImmutableReference.New(""))

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          IsOccupied = false }

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
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, TilePropertiesImmutableReference.New(""))

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          IsOccupied = false }

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

//====================== 

[<Struct>]
type Tile = {
    mutable Health: int
    A: int64
    B: int64
    C: int64
}

let run() =
    let size = 10_000_000
    let tiles = Array.init size (fun _ -> { Health = 0; A = 0L; B = 0L; C = 0L })
    let tiles2 = tiles.AsSpan()

    // Test direct mutation syntax
    let sw1 = Stopwatch.StartNew()
    for i in 0..size-1 do
        tiles[i].Health <- i
    sw1.Stop()

    // Test copy-replace
    let sw2 = Stopwatch.StartNew()
    for i in 0..size-1 do
        tiles[i] <- { tiles[i] with Health = i }
    sw2.Stop()

    let sw3 = Stopwatch.StartNew()
    for i in 0..size-1 do
       tiles2[i].Health <- i
    sw3.Stop()

    printfn "Direct mutation syntax: %dms" sw1.ElapsedMilliseconds
    printfn "Copy-replace:           %dms" sw2.ElapsedMilliseconds
    printfn "Span:                   %dms" sw3.ElapsedMilliseconds

run()    

//================= 
 
let createTestTileProperties () =
    let mutable tileProps = TilePropertiesImmutableReference.New("")
       
    let spriteLoc1 = SpriteLoc(0, 1, 2)
    let properties1 = {
        Walkable = true
        TileType = TileType.Floor
        Health = 100
        DescriptionKey = "floor_description"
        Biome = Biome.Forest
        TileOpacity = TileOpacity.Transparent
        DestroyedSpriteLoc =  Some (SpriteLoc(1, 2, 3))
        NextStateSpriteLoc = None
    }
       
    tileProps <- tileProps.Set(spriteLoc1, properties1)
    tileProps

// Example usage function
let testTilePropertiesSerialization () =
    // Create tile properties
    let originalTileProps = createTestTileProperties()
       
    // Serialize to bytes
    let serializedBytes = TilePropertiesSerializer.serialize originalTileProps
    printfn "Serialized to %d bytes" serializedBytes.Length
       
    // Deserialize back
    let deserializedTileProps = TilePropertiesSerializer.deserialize serializedBytes
       
    // Verify the data
    let spriteLoc = SpriteLoc(0, 1, 2)
    let retrievedProps = deserializedTileProps.[spriteLoc]
       
    printfn "Original tile set name: %s" (originalTileProps.GetTileSetName())
    printfn "Deserialized tile set name: %s" (deserializedTileProps.GetTileSetName())
    printfn "Retrieved properties - Walkable: %b, TileType: %A" retrievedProps.Walkable retrievedProps.TileType
 
    assertEquals retrievedProps.NextStateSpriteLoc originalTileProps[SpriteLoc(0, 1, 2)].NextStateSpriteLoc "StateChangedSpriteLoc"
    assertEquals retrievedProps.DestroyedSpriteLoc originalTileProps[SpriteLoc(0, 1, 2)].DestroyedSpriteLoc "DestroyedSpriteLoc"
       
    deserializedTileProps

testTilePropertiesSerialization ()

//======================

let testPVConjInitialBoundary (n: int) =
    printfn "\n--- Test: PersistentVector Conj/Initial boundary n=%d ---" n
    let pv = PersistentVector.ofSeq [0 .. n - 1]
    assertEquals n pv.Length "pv.Length"
    if n > 0 then
        assertEquals (n - 1) pv.[n - 1] "pv.[n-1]"
        // Initial should drop last
        let pvInit = pv.Initial
        assertEquals (n - 1) pvInit.Length "pv.Initial.Length"
        if n - 1 > 0 then
            assertEquals (n - 2) pvInit.[n - 2] "pv.Initial.[n-2]"
        // Do another initial for extra boundary stress
        if n >= 2 then
            let pvInit2 = pvInit.Initial
            assertEquals (n - 2) pvInit2.Length "pv.Initial.Initial.Length"
            if n - 2 > 0 then
                assertEquals (n - 3) pvInit2.[n - 3] "pv.Initial.Initial.[n-3]"
    printfn "--- TestPVConjInitialBoundary %d: PASSED ---" n

let testPersistentVectorBoundaries () =
    // Exact tail boundary
    testPVConjInitialBoundary 32
    // First element spilling out of tail into tree
    testPVConjInitialBoundary 33
    // Exact 32*32 boundary
    testPVConjInitialBoundary 1024
    // First element spilling to the next level
    testPVConjInitialBoundary 1025

type Change = { I: int; V: int }
let testUpdateMany () =
    printfn "\n--- Test: PersistentVector update APIs (types and behavior) ---"

    // Start with a PersistentVector<int>
    let pv = PersistentVector.ofSeq [0 .. 9]

    // 1) updateMany: updates is seq<int * 'T> (index, value)
    // Here: 'T = int, so we use (index, value) pairs of ints.
    let updatesIdxVal: (int * int) list = [ (0, 123); (5, 777); (9, -1) ]
    let pv2 = PersistentVector.updateMany updatesIdxVal pv

    assertEquals 123 pv2.[0] "updateMany applied at 0"
    assertEquals 777 pv2.[5] "updateMany applied at 5"
    assertEquals -1  pv2.[9] "updateMany applied at 9"
    assertEquals 1   pv2.[1] "other elements unchanged after updateMany"

    // 2) updateManyWith: you choose the update shape 'a and provide a mapper 'a -> (index, value)
    // Example shape: a record type for clarity
    
    let changes: Change list = [ { I = 2; V = 333 }; { I = 7; V = 444 } ]
    let pv3 =
        PersistentVector.updateManyWith (fun ch -> ch.I, ch.V) // mapper: Change -> (index,value)
            changes
            pv2

    assertEquals 333 pv3.[2] "updateManyWith applied at 2"
    assertEquals 444 pv3.[7] "updateManyWith applied at 7"
    assertEquals 123 pv3.[0] "previous updates preserved after updateManyWith"
    assertEquals -1  pv3.[9] "previous updates preserved after updateManyWith"

    // 3) updateManyWithIndexMap: pass an index mapper ('a -> int) and updates as seq<'a * 'T>
    // Here we pick 'a = int, so an index is already an int; use id as the mapper.
    // Updates are then seq<int * int> (indexSource, value).
    let pv4 =
        PersistentVector.updateManyWithIndexMap id
            [ (3, 999); (8, 555) ]
            pv3

    assertEquals 999 pv4.[3] "updateManyWithIndexMap applied at 3"
    assertEquals 555 pv4.[8] "updateManyWithIndexMap applied at 8"

    // Example of compile-time type safety (do not uncomment, this should NOT compile):
    //
    // let badUpdates: (int * string) list = [ (1, "oops") ]
    // let _ = PersistentVector.updateMany badUpdates pv // <- type error: 'T is int, not string
    //
    // let badMap: Change -> int * string = fun ch -> ch.I, string ch.V
    // let _ = PersistentVector.updateManyWith badMap changes pv // <- type error

    printfn "--- Test: PersistentVector update APIs: PASSED ---"

// Run the new tests
testPersistentVectorBoundaries ()
testUpdateMany ()
