#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"
open AspectGameEngine 
open FSharpx.Collections
open System.Reflection
open System
open System.Diagnostics
open AspectGameEngine.Localization
open System.Collections.Generic

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

// NEW: Dummy entity IDs for testing layer cells
let TestFixtureId = 1001
let TestActorId = 2002
let TestItemId = 3003
let TestDecalId = 4004

// NEW: Helper to create an expected empty LayerCell
let makeExpectedEmptyLayerCell () =
    EditorLayerCell.Empty

// --- Test Functions ---
let testResizeWidthOnly () =
    printfn "\n--- Test: Resize Width Only (2x2 -> 3x2) ---"
    let initialVoidSprite = DefaultVoidSprite // Assumes this SpriteLoc variant exists
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, "deftileset")

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 100
          IsOccupied = false } // Assumes ModifiedSprite exists

    let mapWithModifiedTile = initialMap.UpdateTile(0, 0, modifiedTileData)
    
    // NEW: Add a fixture to (1,0) to test layer cell preservation
    let mapWithFixture = mapWithModifiedTile.SetFixture(1, 0, TestFixtureId)
    // NEW: Add an actor to (0,1)
    let mapWithActor = mapWithFixture.SetActor(0, 1, TestActorId)
    // NEW: Add an item to (1,1)
    let mapWithItem = mapWithActor.AddItem(1, 1, TestItemId)
    // NEW: Add a decal to (1,1)
    let mapWithDecal = mapWithItem.AddDecal(1, 1, TestDecalId)

    let resizedMap = mapWithDecal.Resize(3, 2)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap
    let expectedEmptyLayerCell = makeExpectedEmptyLayerCell ()

    assertEquals 3 resizedMap.Width "Width"
    assertEquals 2 resizedMap.Height "Height"
    assertEquals modifiedTileData (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 0)) "New Tile (2,0)"
    
    // NEW: Verify LayerCells
    assertEquals None (resizedMap.GetActor(0,0)) "Actor (0,0) should be None"
    assertEquals None (resizedMap.GetFixture(0,0)) "Fixture (0,0) should be None"
    assertEquals (Some TestFixtureId) (resizedMap.GetFixture(1,0)) "Fixture (1,0) preserved"
    assertEquals (Some TestActorId) (resizedMap.GetActor(0,1)) "Actor (0,1) preserved"
    assertTrue (resizedMap.GetLayerCell(1,1).Items |> List.exists ((=) TestItemId)) "Item (1,1) preserved"
    assertEquals (Some TestDecalId) (resizedMap.GetDecal(1,1)) "Decal (1,1) preserved"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(2,0)) "New LayerCell (2,0) is empty"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(2,1)) "New LayerCell (2,1) is empty"

    printfn "--- TestResizeWidthOnly: PASSED ---"

let testResizeHeightOnly () =
    printfn "\n--- Test: Resize Height Only (2x2 -> 2x3) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, "deftileset")

    let modifiedTileData =
        { SpriteLoc = ModifiedSprite
          Health = 50
          IsOccupied = false }

    let mapWithModifiedTile = initialMap.UpdateTile(1, 1, modifiedTileData)
    
    // NEW: Add a fixture to (0,1) to test layer cell preservation
    let mapWithFixture = mapWithModifiedTile.SetFixture(0, 1, TestFixtureId)
    // NEW: Add an actor to (1,0)
    let mapWithActor = mapWithFixture.SetActor(1, 0, TestActorId)
    // NEW: Add an item to (0,0)
    let mapWithItem = mapWithActor.AddItem(0, 0, TestItemId)
    // NEW: Add a decal to (0,0)
    let mapWithDecal = mapWithItem.AddDecal(0, 0, TestDecalId)

    let resizedMap = mapWithDecal.Resize(2, 3)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap
    let expectedEmptyLayerCell = makeExpectedEmptyLayerCell ()

    assertEquals 2 resizedMap.Width "Width"
    assertEquals 3 resizedMap.Height "Height"
    assertEquals modifiedTileData (resizedMap.GetTile(1, 1)) "Modified Tile (1,1)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(0, 2)) "New Tile (0,2)"
    
    // NEW: Verify LayerCells
    assertEquals (Some TestFixtureId) (resizedMap.GetFixture(0,1)) "Fixture (0,1) preserved"
    assertEquals (Some TestActorId) (resizedMap.GetActor(1,0)) "Actor (1,0) preserved"
    assertTrue (resizedMap.GetLayerCell(0,0).Items |> Seq.contains(TestItemId)) "Item (0,0) preserved"
    assertEquals (Some TestDecalId) (resizedMap.GetDecal(0,0)) "Decal (0,0) preserved"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(0,2)) "New LayerCell (0,2) is empty"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(1,2)) "New LayerCell (1,2) is empty"

    printfn "--- TestResizeHeightOnly: PASSED ---"

let testResizeWidthAndHeight () =
    printfn "\n--- Test: Resize Both (2x2 -> 3x1) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, "deftileset")

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          IsOccupied = false }

    let mapWithModifications = initialMap.UpdateTile(0, 0, modifiedTile00)
    
    // NEW: Add a fixture to (1,0) to test layer cell preservation
    let mapWithFixture = mapWithModifications.SetFixture(1, 0, TestFixtureId)
    // NEW: Add an actor to (0,1) - this will be truncated
    let mapWithActor = mapWithFixture.SetActor(0, 1, TestActorId)
    // NEW: Add a decal to (1,0)
    let mapWithDecal = mapWithActor.AddDecal(1, 0, TestDecalId)

    let resizedMap = mapWithDecal.Resize(3, 1)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap
    let expectedEmptyLayerCell = makeExpectedEmptyLayerCell ()

    assertEquals 3 resizedMap.Width "Width"
    assertEquals 1 resizedMap.Height "Height"
    assertEquals modifiedTile00 (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 0)) "New Tile (2,0)"
    
    // NEW: Verify LayerCells
    assertEquals None (resizedMap.GetActor(0,0)) "Actor (0,0) should be None"
    assertEquals (Some TestFixtureId) (resizedMap.GetFixture(1,0)) "Fixture (1,0) preserved"
    assertEquals (Some TestDecalId) (resizedMap.GetDecal(1,0)) "Decal (1,0) preserved"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(2,0)) "New LayerCell (2,0) is empty"
    
    // Check out-of-bounds access
    try
        let _ = resizedMap.GetTile(0, 1)
        failwith "Accessed out-of-bounds tile"
    with _ ->
        printfn "PASSED: Out-of-bounds tile access failed as expected."
    
    try
        let _ = resizedMap.GetLayerCell(0, 1)
        failwith "Accessed out-of-bounds layer cell"
    with _ ->
        printfn "PASSED: Out-of-bounds layer cell access failed as expected."

    printfn "--- TestResizeWidthAndHeight: PASSED ---"

let testResizeWidthAndHeight2 () =
    printfn "\n--- Test: Resize Both (2x2 -> 3x4) ---"
    let initialVoidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, initialVoidSprite, "deftileset")

    let modifiedTile00 =
        { SpriteLoc = ModifiedSprite
          Health = 200
          IsOccupied = false }

    let mapWithModifications = initialMap.UpdateTile(0, 0, modifiedTile00)
    
    // NEW: Add a fixture to (1,1) to test layer cell preservation
    let mapWithFixture = mapWithModifications.SetFixture(1, 1, TestFixtureId)
    // NEW: Add an actor to (0,1)
    let mapWithActor = mapWithFixture.SetActor(0, 1, TestActorId)
    // NEW: Add an item to (1,0)
    let mapWithItem = mapWithActor.AddItem(1, 0, TestItemId)
    // NEW: Add a decal to (1,0)
    let mapWithDecal = mapWithItem.AddDecal(1, 0, TestDecalId)

    let resizedMap = mapWithDecal.Resize(3, 4)
    let expectedEmptyTileInResized = makeExpectedEmptyTile resizedMap
    let expectedEmptyLayerCell = makeExpectedEmptyLayerCell ()

    assertEquals 3 resizedMap.Width "Width"
    assertEquals 4 resizedMap.Height "Height"
    assertEquals modifiedTile00 (resizedMap.GetTile(0, 0)) "Modified Tile (0,0)"
    assertEquals expectedEmptyTileInResized (resizedMap.GetTile(2, 3)) "New Tile (2,3)"
    
    // NEW: Verify LayerCells
    assertEquals (Some TestFixtureId) (resizedMap.GetFixture(1,1)) "Fixture (1,1) preserved"
    assertEquals (Some TestActorId) (resizedMap.GetActor(0,1)) "Actor (0,1) preserved"
    assertTrue (resizedMap.GetLayerCell(1,0).Items |> Seq.contains(TestItemId)) "Item (1,0) preserved"
    assertEquals (Some TestDecalId) (resizedMap.GetDecal(1,0)) "Decal (1,0) preserved"
    assertEquals expectedEmptyLayerCell (resizedMap.GetLayerCell(2,3)) "New LayerCell (2,3) is empty"
    
    // Check out-of-bounds access
    try
        let _ = resizedMap.GetTile(0, 4)
        failwith "Accessed out-of-bounds tile"
    with _ ->
        printfn "PASSED: Out-of-bounds tile access failed as expected."
    
    try
        let _ = resizedMap.GetLayerCell(0, 4)
        failwith "Accessed out-of-bounds layer cell"
    with _ ->
        printfn "PASSED: Out-of-bounds layer cell access failed as expected."

    printfn "--- TestResizeWidthAndHeight2: PASSED ---" 

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
     
//================= 
 
let createTestTileProperties () =
    let mutable tileProps = TilePropertiesReference("TestTileset") 
       
    let spriteLoc1 = SpriteLoc(0, 1, 2)
    let properties1 = {
        Walkable = true
        Interactable = true 
        TileType = TileType.Floor
        Health = 100
        DescriptionKey = "floor_description"
        Biome = Biome.Forest
        TileOpacity = TileOpacity.Transparent
        DestroyedSpriteLoc =  Some (SpriteLoc(1, 2, 3))
        NextStateSpriteLoc = None
        ComplexState = Some (ComplexState.ClosedDoor { Locked = true }) 
    }
       
    tileProps[spriteLoc1] <- properties1 
    tileProps

// Example usage function
let testTilePropertiesSerialization () =
    printfn "\n--- Test: TileProperties Serialization ---"  
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
    let originalPropsForLoc = originalTileProps.[spriteLoc] 
       
    printfn "Original tile set name: %s" (originalTileProps.TileSetName)
    printfn "Deserialized tile set name: %s" (deserializedTileProps.TileSetName)
    printfn "Retrieved properties - Walkable: %b, TileType: %A, Interactable: %b" retrievedProps.Walkable retrievedProps.TileType retrievedProps.Interactable // CHANGED: Added Interactable to print
 
    assertEquals originalTileProps.TileSetName deserializedTileProps.TileSetName "TileSetName preserved"
    assertEquals originalPropsForLoc.Walkable retrievedProps.Walkable "Walkable preserved"
    assertEquals originalPropsForLoc.Interactable retrievedProps.Interactable "Interactable preserved"  
    assertEquals originalPropsForLoc.TileType retrievedProps.TileType "TileType preserved"
    assertEquals originalPropsForLoc.Health retrievedProps.Health "Health preserved"
    assertEquals originalPropsForLoc.DescriptionKey retrievedProps.DescriptionKey "DescriptionKey preserved"
    assertEquals originalPropsForLoc.Biome retrievedProps.Biome "Biome preserved"
    assertEquals originalPropsForLoc.TileOpacity retrievedProps.TileOpacity "TileOpacity preserved"
    assertEquals originalPropsForLoc.DestroyedSpriteLoc retrievedProps.DestroyedSpriteLoc "DestroyedSpriteLoc preserved"
    assertEquals originalPropsForLoc.NextStateSpriteLoc retrievedProps.NextStateSpriteLoc "NextStateSpriteLoc preserved"
    assertEquals originalPropsForLoc.ComplexState retrievedProps.ComplexState "ComplexState preserved"

    printfn "--- TestTilePropertiesSerialization: PASSED ---"  
    deserializedTileProps

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

//====================== Localization Tests ======================

let testSimpleMessageParsing () =
    printfn "\n--- Test: Simple message parsing ---"
    let aglText = """
ui.hello = "Hello, World!"
ui.greeting = "Hello, {name}!"
"""
    let file = AglParser.Parse(aglText)
    assertEquals 2 file.Entries.Length "Entry count"
    
    match file.Entries.[0].Msg with
    | Simple pieces ->
        assertEquals 1 pieces.Length "ui.hello piece count"
        match pieces.[0] with
        | Text t -> assertEquals "Hello, World!" t "ui.hello text"
        | _ -> failwith "Expected Text piece"
    | _ -> failwith "Expected Simple message"
    
    match file.Entries.[1].Msg with
    | Simple pieces ->
        assertEquals 3 pieces.Length "ui.greeting piece count"
        match pieces with
        | [Text "Hello, "; Hole ph; Text "!"] ->
            assertEquals "name" ph.Name "placeholder name"
            assertTrue (ph.Format = ValueNone) "no format"
        | _ -> failwith "Unexpected piece structure"
    | _ -> failwith "Expected Simple message"
    
    printfn "--- testSimpleMessageParsing: PASSED ---"

let testPlaceholderWithFormat () =
    printfn "\n--- Test: Placeholder with format ---"
    let aglText = @"shop.price = ""Price: {amount:C2}"""
    let file = AglParser.Parse(aglText)
    
    match file.Entries.[0].Msg with
    | Simple pieces ->
        match pieces with
        | [Text "Price: "; Hole ph] ->
            assertEquals "amount" ph.Name "placeholder name"
            assertEquals (ValueSome "C2") ph.Format "format specifier"
        | _ -> failwith "Unexpected structure"
    | _ -> failwith "Expected Simple"
    
    printfn "--- testPlaceholderWithFormat: PASSED ---"

let testPluralMessage () =
    printfn "\n--- Test: Plural message ---"
    let aglText = """
inv.items(count) {
  =0: "No items"
  =1: "One item"
  other: "{count} items"
}
"""
    let file = AglParser.Parse(aglText)
    assertEquals 1 file.Entries.Length "Entry count"
    
    match file.Entries.[0].Msg with
    | Plural (sel, vars) ->
        assertEquals "count" sel "selector name"
        assertEquals 3 vars.Length "variant count"
        
        match vars.[0].Label with
        | Exact 0 -> ()
        | _ -> failwith "Expected Exact 0"
        
        match vars.[1].Label with
        | Exact 1 -> ()
        | _ -> failwith "Expected Exact 1"
        
        match vars.[2].Label with
        | Category "other" -> ()
        | _ -> failwith "Expected Category 'other'"
        
        // Check that 'other' variant has placeholder
        match vars.[2].Message with
        | [Hole ph; Text " items"] ->
            assertEquals "count" ph.Name "count placeholder"
        | _ -> failwith "Unexpected 'other' message structure"
    | _ -> failwith "Expected Plural message"
    
    printfn "--- testPluralMessage: PASSED ---"

let testSelectMessage () =
    printfn "\n--- Test: Select message ---"
    let aglText = """
char.pronoun(gender) {
  male: "he"
  female: "she"
  other: "they"
}
"""
    let file = AglParser.Parse(aglText)
    
    match file.Entries.[0].Msg with
    | Select (sel, vars) ->
        assertEquals "gender" sel "selector"
        assertEquals 3 vars.Length "variant count"
        
        let labels = vars |> Array.map (fun v -> 
            match v.Label with
            | Category c -> c
            | _ -> failwith "Expected Category")
        
        assertTrue (labels |> Array.contains "male") "has male"
        assertTrue (labels |> Array.contains "female") "has female"
        assertTrue (labels |> Array.contains "other") "has other"
    | _ -> failwith "Expected Select"
    
    printfn "--- testSelectMessage: PASSED ---"

let testAlias () =
    printfn "\n--- Test: Alias ---"
    let aglText = """
ui.play = "Play"
menu.start = @ui.play
"""
    let file = AglParser.Parse(aglText)
    assertEquals 2 file.Entries.Length "Entry count"
    
    match file.Entries.[1].Msg with
    | Alias target -> assertEquals "ui.play" target "alias target"
    | _ -> failwith "Expected Alias"
    
    printfn "--- testAlias: PASSED ---"

let testTripleQuotedString () =
    printfn "\n--- Test: Triple-quoted multiline ---"
    let aglText = "lore.intro = \"\"\"\nLine 1\nLine 2\n\"\"\""
    let file = AglParser.Parse(aglText)
    
    match file.Entries.[0].Msg with
    | Simple [Text t] ->
        assertTrue (t.Contains("Line 1")) "has Line 1"
        assertTrue (t.Contains("Line 2")) "has Line 2"
    | _ -> failwith "Expected Simple with text"
    
    printfn "--- testTripleQuotedString: PASSED ---"

let testEscapeSequences () =
    printfn "\n--- Test: Escape sequences ---"
    let aglText = @"test.escapes = ""Newline:\nTab:\tQuote:\""Brace:\\{"""
    let file = AglParser.Parse(aglText)
    
    match file.Entries.[0].Msg with
    | Simple [Text t] ->
        assertTrue (t.Contains("\n")) "has newline"
        assertTrue (t.Contains("\t")) "has tab"
        assertTrue (t.Contains("\"")) "has quote"
        assertTrue (t.Contains("{")) "has brace"
    | _ -> failwith "Expected Simple"
    
    printfn "--- testEscapeSequences: PASSED ---"

let testMeta () =
    printfn "\n--- Test: Meta header ---"
    let aglText = """
[meta]
locale = "en-US"
fallback = ["en"]

ui.test = "Test"
"""
    let file = AglParser.Parse(aglText)
    
    match file.Meta.Locale with
    | ValueSome "en-US" -> ()
    | _ -> failwith "Expected locale en-US"
    
    assertEquals 1 file.Meta.Fallback.Length "fallback count"
    assertEquals "en" file.Meta.Fallback.[0] "fallback locale"
    
    printfn "--- testMeta: PASSED ---"

let testDuplicateKeyDetection () =
    printfn "\n--- Test: Duplicate key detection ---"
    let aglText = """
ui.test = "First"
ui.test = "Second"
"""
    try
        let _ = AglParser.Parse(aglText)
        failwith "Should have thrown on duplicate key"
    with ex ->
        assertTrue (ex.Message.Contains("Duplicate")) "error mentions duplicate"
    
    printfn "--- testDuplicateKeyDetection: PASSED ---"

let testMissingOtherVariant () =
    printfn "\n--- Test: Missing 'other' variant detection ---"
    let aglText = """
test.select(x) {
  foo: "Foo"
  bar: "Bar"
}
"""
    try
        let _ = AglParser.Parse(aglText)
        failwith "Should have thrown on missing 'other'"
    with ex ->
        assertTrue (ex.Message.Contains("other")) "error mentions 'other'"
    
    printfn "--- testMissingOtherVariant: PASSED ---"

let runLocalizationParserTests () =
    printfn "\n=== Running Localization Parser Tests ==="
    // Run localization tests
    testSimpleMessageParsing ()
    testPlaceholderWithFormat ()
    testPluralMessage ()
    testSelectMessage ()
    testAlias ()
    testTripleQuotedString ()
    testEscapeSequences ()
    testMeta ()
    testDuplicateKeyDetection ()
    testMissingOtherVariant ()

    printfn "\n=== All Localization Tests Passed ==="
 
//========

module ZigZag =
    let inline encode (value: int) =
        int ((uint32 value <<< 1) ^^^ (uint32 (value >>> 31)))
 
    let inline decode (value: int) =
        let u = uint32 value
        let shifted = int (u >>> 1)
        let sign = int (u &&& 1u)
        shifted ^^^ -sign

let testBinaryPackRoundTrip () =
    printfn "\n--- Test: Binary pack round-trip ---"
    let aglText = """
[meta]
locale = "en-US"
fallback = ["en"]

ui.hello = "Hello, World!"
ui.greeting = "Hello, {name}!"
shop.price = "Price: {amount:C2}"

inv.items(count) {
  =0: "No items"
  =1: "One item"
  other: "{count} items"
}

char.pronoun(gender) {
  male: "he"
  female: "she"
  other: "they"
}

ui.quit = "Quit"
menu.exit = @ui.quit
"""
    
    // Parse -> Pack -> Binary -> Unpack
    let file = AglParser.Parse(aglText)
    let pack1 = AglPacker.Build(file)
    let bytes = AglPacker.WriteBinary(pack1)
    let pack2 = AglPacker.ReadBinary(bytes)
    
    // Verify basic properties
    assertEquals "en-US" pack2.Locale "Locale preserved"
    assertEquals pack1.Keys.Length pack2.Keys.Length "Key count preserved"
    
    // Verify simple message
    let idx = pack2.Index.["ui.hello"]
    match pack2.Messages.[idx] with
    | Simple pieces ->
        match pieces with
        | [Text "Hello, World!"] -> ()
        | _ -> failwith "ui.hello content mismatch"
    | _ -> failwith "Expected Simple for ui.hello"
    
    // Verify message with placeholder and format
    let idx2 = pack2.Index.["shop.price"]
    match pack2.Messages.[idx2] with
    | Simple pieces ->
        match pieces with
        | [Text "Price: "; Hole ph] ->
            assertEquals "amount" ph.Name "placeholder name"
            assertEquals (ValueSome "C2") ph.Format "format preserved"
        | _ -> failwith "shop.price structure mismatch"
    | _ -> failwith "Expected Simple for shop.price"
    
    // Verify plural
    let idx3 = pack2.Index.["inv.items"]
    match pack2.Messages.[idx3] with
    | Plural (sel, vars) ->
        assertEquals "count" sel "selector preserved"
        assertEquals 3 vars.Length "variant count preserved"
        
        match vars.[0].Label with
        | Exact 0 -> ()
        | _ -> failwith "Expected Exact 0"
        
        match vars.[2].Label with
        | Category "other" -> ()
        | _ -> failwith "Expected Category 'other'"
        
        // Verify placeholder in 'other' variant
        match vars.[2].Message with
        | [Hole ph; Text " items"] ->
            assertEquals "count" ph.Name "count placeholder preserved"
        | _ -> failwith "Plural 'other' content mismatch"
    | _ -> failwith "Expected Plural for inv.items"
    
    // Verify select
    let idx4 = pack2.Index.["char.pronoun"]
    match pack2.Messages.[idx4] with
    | Select (sel, vars) ->
        assertEquals "gender" sel "selector preserved"
        assertEquals 3 vars.Length "variant count preserved"
        let labels = vars |> Array.map (fun v ->
            match v.Label with
            | Category c -> c
            | _ -> failwith "Expected Category")
        assertTrue (labels |> Array.contains "male") "has male"
        assertTrue (labels |> Array.contains "female") "has female"
        assertTrue (labels |> Array.contains "other") "has other"
    | _ -> failwith "Expected Select for char.pronoun"
    
    // Verify alias
    let idx5 = pack2.Index.["menu.exit"]
    match pack2.Messages.[idx5] with
    | Alias target -> assertEquals "ui.quit" target "alias target preserved"
    | _ -> failwith "Expected Alias for menu.exit"
    
    printfn "Binary size: %d bytes" bytes.Length
    printfn "--- testBinaryPackRoundTrip: PASSED ---"

let testZigZagEncoding () =
    printfn "\n--- Test: ZigZag encoding ---"
    
    // Test some known values
    let test n expected =
        let encoded = ZigZag.encode n
        assertEquals expected encoded (sprintf "ZigZag.encode(%d)" n)
        let decoded = ZigZag.decode encoded
        assertEquals n decoded (sprintf "ZigZag.decode(%d)" encoded)
    
    test 0 0
    test -1 1
    test 1 2
    test -2 3
    test 2 4
    test 2147483647 -2  // max int32
    test -2147483648 -1 // min int32
    
    printfn "--- testZigZagEncoding: PASSED ---"

let testEmptyPack () =
    printfn "\n--- Test: Empty pack ---"
    let aglText = """
[meta]
locale = "test"
"""
    let file = AglParser.Parse(aglText)
    let pack = AglPacker.Build(file)
    assertEquals 0 pack.Keys.Length "Empty pack has no keys"
    assertEquals "test" pack.Locale "Locale still present"
    
    let bytes = AglPacker.WriteBinary(pack)
    let pack2 = AglPacker.ReadBinary(bytes)
    assertEquals 0 pack2.Keys.Length "Round-trip empty pack"
    
    printfn "--- testEmptyPack: PASSED ---"

let runLocalizationPackerTests () =
    printfn "\n=== Running Localization Packer Tests ==="
    testBinaryPackRoundTrip ()
    testZigZagEncoding ()
    testEmptyPack ()
    printfn "\n=== All Localization Packer Tests Passed ===" 

//==========================

let testLocalizerSimpleGet () =
    printfn "\n--- Test: Localizer simple Get ---"
    let aglText = """
ui.hello = "Hello, World!"
ui.missing = "Fallback text"
"""
    let loc = Localization.loadAgl aglText
    
    assertEquals "Hello, World!" (loc.Get("ui.hello")) "Get existing key"
    assertEquals "ui.notfound" (loc.Get("ui.notfound")) "Get missing key returns key"
    
    match loc.TryGet("ui.hello") with
    | ValueSome s -> assertEquals "Hello, World!" s "TryGet existing"
    | _ -> failwith "Expected ValueSome"
    
    match loc.TryGet("ui.notfound") with
    | ValueNone -> ()
    | _ -> failwith "Expected ValueNone for missing key"
    
    printfn "--- testLocalizerSimpleGet: PASSED ---"

let rodict d = Dictionary (dict d) :> IReadOnlyDictionary<string, _>

let testLocalizerFormat () =
    printfn "\n--- Test: Localizer Format with args ---"
    let aglText = """
ui.greeting = "Hello, {name}!"
shop.price = "Price: {amount:C2}"
"""
    let loc = Localization.loadAgl aglText
    
    let args1 = Dictionary (dict [ "name", box "Alice" ])
    assertEquals "Hello, Alice!" (loc.Format("ui.greeting", args1)) "Format with name arg"
    
    let args2 = rodict [ "amount", box 42.50 ]
    let result = loc.Format("shop.price", args2)
    assertTrue (result.Contains("42.5") || result.Contains("42,5")) "Format with C2 currency"
    
    // Missing placeholder - should show {placeholder}
    let args3 = rodict []
    let result3 = loc.Format("ui.greeting", args3)
    assertTrue (result3.Contains("{name}")) "Missing arg shows placeholder"
    
    printfn "--- testLocalizerFormat: PASSED ---"

let testLocalizerPlural () =
    printfn "\n--- Test: Localizer Plural ---"
    let aglText = """
inv.items(count) {
  =0: "No items"
  =1: "One item"
  other: "{count} items"
}
"""
    let loc = Localization.loadAgl aglText
    
    let args = rodict []
    assertEquals "No items" (loc.Plural("inv.items", 0L, args)) "Plural for 0"
    assertEquals "One item" (loc.Plural("inv.items", 1L, args)) "Plural for 1"
    
    let result2 = loc.Plural("inv.items", 2L, args)
    assertTrue (result2.Contains("2")) "Plural for 2 includes count"
    assertTrue (result2.Contains("items")) "Plural for 2 has 'items'"
    
    let result99 = loc.Plural("inv.items", 99L, args)
    assertTrue (result99.Contains("99")) "Plural for 99 includes count"
    
    printfn "--- testLocalizerPlural: PASSED ---"

let testLocalizerSelect () =
    printfn "\n--- Test: Localizer Select ---"
    let aglText = """
char.pronoun(gender) {
  male: "he"
  female: "she"
  other: "they"
}

char.greeting(gender) {
  male: "Hello, Mr. {name}"
  female: "Hello, Ms. {name}"
  other: "Hello, {name}"
}
"""
    let loc = Localization.loadAgl aglText
    
    let args = rodict []
    assertEquals "he" (loc.Select("char.pronoun", "male", args)) "Select male"
    assertEquals "she" (loc.Select("char.pronoun", "female", args)) "Select female"
    assertEquals "they" (loc.Select("char.pronoun", "other", args)) "Select other"
    assertEquals "they" (loc.Select("char.pronoun", "unknown", args)) "Select unknown falls back to other"
    
    let args2 = rodict [ "name", box "Jordan" ]
    let result = loc.Select("char.greeting", "male", args2)
    assertTrue (result.Contains("Mr.")) "Select with args includes variant text"
    assertTrue (result.Contains("Jordan")) "Select with args includes placeholder"
    
    printfn "--- testLocalizerSelect: PASSED ---"

let testLocalizerAlias () =
    printfn "\n--- Test: Localizer Alias resolution ---"
    let aglText = """
ui.play = "Play Game"
menu.start = @ui.play
menu.begin = @menu.start
"""
    let loc = Localization.loadAgl aglText
    
    assertEquals "Play Game" (loc.Get("ui.play")) "Direct key"
    assertEquals "Play Game" (loc.Get("menu.start")) "Single alias"
    assertEquals "Play Game" (loc.Get("menu.begin")) "Chained alias"
    
    printfn "--- testLocalizerAlias: PASSED ---"

let testLocalizerAliasCycle () =
    printfn "\n--- Test: Localizer Alias cycle detection ---"
    let aglText = """
a.key = @b.key
b.key = @c.key
c.key = @a.key
"""
    let loc = Localization.loadAgl aglText
    
    // Should return key itself when cycle detected (not infinite loop)
    let result = loc.Get("a.key")
    assertEquals "a.key" result "Cycle detection returns key"
    
    printfn "--- testLocalizerAliasCycle: PASSED ---"

let testLocalizerFallback () =
    printfn "\n--- Test: Localizer with fallback packs ---"
    let primaryText = """
[meta]
locale = "fr-FR"

ui.hello = "Bonjour"
"""
    let fallbackText = """
[meta]
locale = "en-US"

ui.hello = "Hello"
ui.goodbye = "Goodbye"
"""
    let primary = AglParser.Parse(primaryText) |> AglPacker.Build
    let fallback = AglParser.Parse(fallbackText) |> AglPacker.Build
    
    let loc = Localizer(primary, fallbacks = [| fallback |])
    
    assertEquals "Bonjour" (loc.Get("ui.hello")) "Primary takes precedence"
    assertEquals "Goodbye" (loc.Get("ui.goodbye")) "Falls back for missing key"
    assertEquals "ui.missing" (loc.Get("ui.missing")) "Missing in all returns key"
    
    printfn "--- testLocalizerFallback: PASSED ---"

let testLocalizerBinaryRoundTrip () =
    printfn "\n--- Test: Localizer from binary ---"
    let aglText = """
ui.test = "Test {value}"
inv.items(count) {
  =0: "Empty"
  other: "{count} items"
}
"""
    let file = AglParser.Parse(aglText)
    let pack = AglPacker.Build(file)
    let bytes = AglPacker.WriteBinary(pack)
    
    let loc = Localization.fromBinary bytes
    
    let args = rodict [ "value", box 123 ]
    let result = loc.Format("ui.test", args)
    assertTrue (result.Contains("123")) "Binary load preserves format"
    
    assertEquals "Empty" (loc.Plural("inv.items", 0L, rodict [])) "Binary load preserves plural"
    
    printfn "--- testLocalizerBinaryRoundTrip: PASSED ---"

let runLocalizerTests () =
    printfn "\n=== Running Localizer Tests ==="
    testLocalizerSimpleGet ()
    testLocalizerFormat ()
    testLocalizerPlural ()
    testLocalizerSelect ()
    testLocalizerAlias ()
    testLocalizerAliasCycle ()
    testLocalizerFallback ()
    testLocalizerBinaryRoundTrip ()
    printfn "\n=== All Localizer Tests Passed ==="

//==============

let testMigrateEntitySpriteType () =
    printfn "\n--- Test: Migrate Entity Sprite Type ---"
    let voidSprite = DefaultVoidSprite
    let initialMap = EditorTileMap.New(2, 2, voidSprite, "deftileset")
    
    let entityId = 500
    // Define types for migration
    let actorType = SpriteType.Actor { TileOpacity = TileOpacity.Opaque; DescKey = "" }
    let fixtureType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; DescKey = ""; TileOpacity = TileOpacity.Opaque }
    let decalType = SpriteType.Decal { Interactable = false; DescKey = "" }
    let itemType = SpriteType.Item { DescKey = "" }

    // --- Scenario 1: Fixture -> Actor (Success) ---
    printfn "Scenario 1: Fixture -> Actor (Empty Dest)"
    let map1 = initialMap.SetFixture(0, 0, entityId)
    let migrated1 = map1.MigrateEntitySpriteType(entityId, fixtureType, actorType)
    
    assertEquals None (migrated1.GetFixture(0, 0)) "Fixture slot cleared"
    assertEquals (Some entityId) (migrated1.GetActor(0, 0)) "Actor slot now occupied"

    // --- Scenario 2: Actor -> Fixture (Conflict / Destroy) ---
    printfn "Scenario 2: Actor -> Fixture (Destination Occupied)"
    let blockerId = 999
    // Tile (0,0) has our entity in Actor slot, and a blocker in Fixture slot
    let map2 = initialMap.SetActor(0, 0, entityId).SetFixture(0, 0, blockerId)
    let migrated2 = map2.MigrateEntitySpriteType(entityId, actorType, fixtureType)
    
    assertEquals None (migrated2.GetActor(0, 0)) "Entity removed from old Actor slot"
    assertEquals (Some blockerId) (migrated2.GetFixture(0, 0)) "Fixture slot still held by blocker; entity 500 destroyed"

    // --- Scenario 3: Fixture -> Item (Cross Boundary / Destroy) ---
    printfn "Scenario 3: Fixture -> Item (Destruction Rule)"
    let map3 = initialMap.SetFixture(0, 0, entityId)
    let migrated3 = map3.MigrateEntitySpriteType(entityId, fixtureType, itemType)
    
    assertEquals None (migrated3.GetFixture(0, 0)) "Fixture slot cleared"
    let cell3 = migrated3.GetLayerCell(0, 0)
    assertTrue (not (cell3.Items |> List.contains entityId)) "Entity not added to items list (boundary rule)"

    // --- Scenario 4: Item -> Decal (Cross Boundary / Destroy) ---
    printfn "Scenario 4: Item -> Decal (Destruction Rule)"
    let map4 = initialMap.AddItem(0, 0, entityId)
    let migrated4 = map4.MigrateEntitySpriteType(entityId, itemType, decalType)
    
    let cell4 = migrated4.GetLayerCell(0, 0)
    assertTrue (not (cell4.Items |> List.contains entityId)) "Item list cleared"
    assertEquals None (migrated4.GetDecal(0, 0)) "Decal slot NOT occupied (boundary rule)"

    // --- Scenario 5: Multi-tile migration ---
    printfn "Scenario 5: Multi-tile update"
    let map5 = initialMap.SetFixture(0, 0, entityId).SetFixture(1, 1, entityId)
    let migrated5 = map5.MigrateEntitySpriteType(entityId, fixtureType, actorType)
    
    assertEquals (Some entityId) (migrated5.GetActor(0, 0)) "Tile (0,0) migrated"
    assertEquals (Some entityId) (migrated5.GetActor(1, 1)) "Tile (1,1) migrated"
    assertEquals None (migrated5.GetFixture(0, 0)) "Tile (0,0) fixture slot empty"

    printfn "--- TestMigrateEntitySpriteType: PASSED ---"

//---------
 
let testMigrateLargeVolume () =
    printfn "\n--- Test: Large Volume Migration Stress (1000 tiles) ---"
    let width, height = 100, 100
    let map = EditorTileMap.New(width, height, DefaultVoidSprite, "deftileset")
    let entityId = 888
    let itemType = SpriteType.Item { DescKey = "" }
    let fixtureType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; DescKey = ""; TileOpacity = TileOpacity.Opaque }

    // Place the entity in 1000 locations (first 10 rows)
    let swPop = Stopwatch.StartNew()
    let mutable mapWithItems = map
    for y in 0 .. 9 do
        for x in 0 .. 99 do
            mapWithItems <- mapWithItems.AddItem(x, y, entityId)
    swPop.Stop()
    
    printfn "Populated 1000 tiles with entity %d in %dms" entityId swPop.ElapsedMilliseconds
    
    let sw = Stopwatch.StartNew()
    let migrated = mapWithItems.MigrateEntitySpriteType(entityId, itemType, fixtureType)
    sw.Stop()
    
    printfn "Migrated 1000 tiles in %dms" sw.ElapsedMilliseconds

    // Verify cleanup (using the destroy rule for Item boundary crossing)
    let mutable foundCount = 0
    for i in 0 .. (width * height) - 1 do
        let cell = migrated.LayerCells.[i]
        if cell.Items |> List.contains entityId || cell.FixtureId = Some entityId then
            foundCount <- foundCount + 1
            
    assertEquals 0 foundCount "Entity should be completely removed from all 1000 tiles due to destroy rule"
    printfn "--- TestMigrateLargeVolume: PASSED ---"

let testMigrateLargeVolumeSuccess () =
    printfn "\n--- Test: Large Volume Migration Success (1000 tiles, No Destruction) ---"
    let width, height = 100, 100
    let map = EditorTileMap.New(width, height, DefaultVoidSprite, "deftileset")
    let entityId = 777
    let fixtureType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; DescKey = ""; TileOpacity = TileOpacity.Opaque }
    let actorType = SpriteType.Actor { TileOpacity = TileOpacity.Opaque; DescKey = "" }

    // Place the entity in 1000 Fixture slots
    let swPop = Stopwatch.StartNew()
    let mutable mapWithFixtures = map
    for y in 0 .. 9 do
        for x in 0 .. 99 do
            mapWithFixtures <- mapWithFixtures.SetFixture(x, y, entityId)
    swPop.Stop()
    
    printfn "Populated 1000 tiles with Fixture %d in %dms" entityId swPop.ElapsedMilliseconds
    
    let sw = Stopwatch.StartNew()
    let migrated = mapWithFixtures.MigrateEntitySpriteType(entityId, fixtureType, actorType)
    sw.Stop()
    
    printfn "Migrated 1000 tiles in %dms" sw.ElapsedMilliseconds

    // Verify preservation
    let mutable successCount = 0
    let mutable failureCount = 0
    for y in 0 .. 9 do
        for x in 0 .. 99 do
            let cell = migrated.GetLayerCell(x, y)
            if cell.ActorId = Some entityId && cell.FixtureId = None then
                successCount <- successCount + 1
            else
                failureCount <- failureCount + 1
            
    assertEquals 1000 successCount "1000 entities should have successfully moved to Actor slot"
    assertEquals 0 failureCount "No entities should have been destroyed"
    printfn "--- TestMigrateLargeVolumeSuccess: PASSED ---"
  


//testResizeWidthOnly ()
//testResizeHeightOnly ()
//testResizeWidthAndHeight ()
//testResizeWidthAndHeight2 ()
////----
//testTilePropertiesSerialization ()
////----
//testPersistentVectorBoundaries ()
//testUpdateMany ()
////---
//runLocalizationParserTests ()
//runLocalizationPackerTests ()  
//runLocalizerTests ()
////---
//testMigrateEntitySpriteType ()
//testMigrateLargeVolume ()  
//testMigrateLargeVolumeSuccess ()


let runAllTests() =  
    printfn "\n=== Running All Tests ==="
    run()    
    testResizeWidthOnly ()
    testResizeHeightOnly ()
    testResizeWidthAndHeight ()
    testResizeWidthAndHeight2 ()
    testTilePropertiesSerialization () |> ignore
    testPersistentVectorBoundaries ()
    testUpdateMany ()
    runLocalizationParserTests ()
    runLocalizationPackerTests () 
    runLocalizerTests ()
    testMigrateEntitySpriteType ()
    testMigrateLargeVolume ()  
    testMigrateLargeVolumeSuccess ()
    printfn "\n=== All Tests Completed (Passed) ==="

let runJustLoclizerTests() =   
    runLocalizationParserTests ()
    runLocalizationPackerTests () 
    runLocalizerTests ()

let runAllButLocalizerTests() =
    testResizeWidthOnly ()
    testResizeHeightOnly ()
    testResizeWidthAndHeight ()
    testResizeWidthAndHeight2 ()
    testTilePropertiesSerialization () |> ignore
    testPersistentVectorBoundaries ()
    testUpdateMany ()
    testMigrateEntitySpriteType ()
    testMigrateLargeVolume ()  
    testMigrateLargeVolumeSuccess ()

//runJustLoclizerTests() 
//runAllButLocalizerTests()

runAllTests()
    

    