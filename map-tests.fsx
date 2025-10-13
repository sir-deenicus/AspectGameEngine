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

//====================== TileMap Serialization Tests ======================

let createTestTileMap () =
    printfn "\n--- Creating Test TileMap ---"
    
    // Create tile properties
    let mutable tileProps = TilePropertiesImmutableReference.New("TestTileSet")
    
    let floorSprite = SpriteLoc(0, 1, 1)
    let wallSprite = SpriteLoc(0, 2, 2)
    let doorSprite = SpriteLoc(0, 3, 3)
    let voidSprite = SpriteLoc(0, 0, 0)
    
    let floorProps = {
        Walkable = true
        TileType = TileType.Floor
        Health = 100
        DescriptionKey = "floor_stone"
        Biome = Biome.None
        TileOpacity = TileOpacity.Transparent
        DestroyedSpriteLoc = None
        NextStateSpriteLoc = None
    }
    
    let wallProps = {
        Walkable = false
        TileType = TileType.Wall
        Health = 500
        DescriptionKey = "wall_stone"
        Biome = Biome.Mountain
        TileOpacity = TileOpacity.Opaque
        DestroyedSpriteLoc = Some (SpriteLoc(0, 2, 4))
        NextStateSpriteLoc = None
    }
    
    let doorProps = {
        Walkable = true
        TileType = TileType.Door
        Health = 150
        DescriptionKey = "door_wood"
        Biome = Biome.None
        TileOpacity = TileOpacity.Transparent
        DestroyedSpriteLoc = Some (SpriteLoc(0, 3, 4))
        NextStateSpriteLoc = Some (SpriteLoc(0, 3, 5))
    }
    
    tileProps <- tileProps.Set(floorSprite, floorProps)
    tileProps <- tileProps.Set(wallSprite, wallProps)
    tileProps <- tileProps.Set(doorSprite, doorProps)
    
    // Convert to mutable reference
    let tilePropsRef = TilePropertiesReference()
    for KeyValue(spriteLoc, props) in tileProps.Properties do
        tilePropsRef.[spriteLoc] <- props
    
    // Create a 3x3 map with various tiles
    let tiles = [|
        // Row 0
        { SpriteLoc = wallSprite; Health = 500; IsOccupied = false }
        { SpriteLoc = wallSprite; Health = 500; IsOccupied = false }
        { SpriteLoc = wallSprite; Health = 500; IsOccupied = false }
        // Row 1
        { SpriteLoc = wallSprite; Health = 500; IsOccupied = false }
        { SpriteLoc = floorSprite; Health = 100; IsOccupied = true }
        { SpriteLoc = doorSprite; Health = 150; IsOccupied = false }
        // Row 2
        { SpriteLoc = wallSprite; Health = 450; IsOccupied = false }
        { SpriteLoc = floorSprite; Health = 95; IsOccupied = false }
        { SpriteLoc = wallSprite; Health = 500; IsOccupied = false }
    |]
    
    TileMap(3, 3, tiles, voidSprite, tilePropsRef, "TestDungeon", MapType.Dungeon)

let testTileMapBasicSerialization () =
    printfn "\n--- Test: TileMap Basic Serialization ---"
    
    let originalMap = createTestTileMap()
    
    // Serialize
    let serializedBytes = TileMapSerializer.serialize originalMap
    printfn "Serialized TileMap to %d bytes" serializedBytes.Length
    
    // Deserialize
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    
    // Verify basic properties
    assertEquals originalMap.Width deserializedMap.Width "Width"
    assertEquals originalMap.Height deserializedMap.Height "Height"
    assertEquals originalMap.MapName deserializedMap.MapName "MapName"
    assertEquals originalMap.MapType deserializedMap.MapType "MapType"
    assertEquals originalMap.VoidSpriteLoc deserializedMap.VoidSpriteLoc "VoidSpriteLoc"
    
    printfn "--- TileMap Basic Serialization: PASSED ---"

let testTileMapTileData () =
    printfn "\n--- Test: TileMap Tile Data Integrity ---"
    
    let originalMap = createTestTileMap()
    let serializedBytes = TileMapSerializer.serialize originalMap
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    
    // Check all tiles
    for y in 0 .. originalMap.Height - 1 do
        for x in 0 .. originalMap.Width - 1 do
            let origTile = originalMap.GetTile(x, y)
            let deserTile = deserializedMap.GetTile(x, y)
            
            assertEquals origTile.SpriteLoc deserTile.SpriteLoc (sprintf "Tile (%d,%d) SpriteLoc" x y)
            assertEquals origTile.Health deserTile.Health (sprintf "Tile (%d,%d) Health" x y)
            assertEquals origTile.IsOccupied deserTile.IsOccupied (sprintf "Tile (%d,%d) IsOccupied" x y)
    
    printfn "--- TileMap Tile Data Integrity: PASSED ---"

let testTileMapPropertiesIntegrity () =
    printfn "\n--- Test: TileMap Properties Integrity ---"
    
    let originalMap = createTestTileMap()
    let serializedBytes = TileMapSerializer.serialize originalMap
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    
    // Test specific sprite locations with known properties
    let floorSprite = SpriteLoc(0, 1, 1)
    let wallSprite = SpriteLoc(0, 2, 2)
    let doorSprite = SpriteLoc(0, 3, 3)
    
    // Floor properties
    let origFloorProps = originalMap.TilePropertiesReference.[floorSprite]
    let deserFloorProps = deserializedMap.TilePropertiesReference.[floorSprite]
    
    assertEquals origFloorProps.Walkable deserFloorProps.Walkable "Floor Walkable"
    assertEquals origFloorProps.TileType deserFloorProps.TileType "Floor TileType"
    assertEquals origFloorProps.Health deserFloorProps.Health "Floor Health"
    assertEquals origFloorProps.DescriptionKey deserFloorProps.DescriptionKey "Floor DescriptionKey"
    assertEquals origFloorProps.Biome deserFloorProps.Biome "Floor Biome"
    assertEquals origFloorProps.TileOpacity deserFloorProps.TileOpacity "Floor TileOpacity"
    assertEquals origFloorProps.DestroyedSpriteLoc deserFloorProps.DestroyedSpriteLoc "Floor DestroyedSpriteLoc"
    assertEquals origFloorProps.NextStateSpriteLoc deserFloorProps.NextStateSpriteLoc "Floor NextStateSpriteLoc"
    
    // Wall properties (with DestroyedSpriteLoc)
    let origWallProps = originalMap.TilePropertiesReference.[wallSprite]
    let deserWallProps = deserializedMap.TilePropertiesReference.[wallSprite]
    
    assertEquals origWallProps.Walkable deserWallProps.Walkable "Wall Walkable"
    assertEquals origWallProps.TileType deserWallProps.TileType "Wall TileType"
    assertEquals origWallProps.DestroyedSpriteLoc deserWallProps.DestroyedSpriteLoc "Wall DestroyedSpriteLoc"
    assertTrue deserWallProps.DestroyedSpriteLoc.IsSome "Wall has DestroyedSpriteLoc"
    
    // Door properties (with both optional sprite locs)
    let origDoorProps = originalMap.TilePropertiesReference.[doorSprite]
    let deserDoorProps = deserializedMap.TilePropertiesReference.[doorSprite]
    
    assertEquals origDoorProps.TileType deserDoorProps.TileType "Door TileType"
    assertEquals origDoorProps.DestroyedSpriteLoc deserDoorProps.DestroyedSpriteLoc "Door DestroyedSpriteLoc"
    assertEquals origDoorProps.NextStateSpriteLoc deserDoorProps.NextStateSpriteLoc "Door NextStateSpriteLoc"
    assertTrue deserDoorProps.DestroyedSpriteLoc.IsSome "Door has DestroyedSpriteLoc"
    assertTrue deserDoorProps.NextStateSpriteLoc.IsSome "Door has NextStateSpriteLoc"
    
    printfn "--- TileMap Properties Integrity: PASSED ---"

let testTileMapEmptyMap () =
    printfn "\n--- Test: TileMap Empty/Minimal Map ---"
    
    let voidSprite = SpriteLoc(0, 0, 0)
    let tilePropsRef = TilePropertiesReference()
    let emptyTiles = Array.create 1 { SpriteLoc = voidSprite; Health = 0; IsOccupied = false }
    
    let emptyMap = TileMap(1, 1, emptyTiles, voidSprite, tilePropsRef, "Empty", MapType.Room)
    
    let serializedBytes = TileMapSerializer.serialize emptyMap
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    
    assertEquals 1 deserializedMap.Width "Empty map Width"
    assertEquals 1 deserializedMap.Height "Empty map Height"
    assertEquals "Empty" deserializedMap.MapName "Empty map Name"
    assertEquals MapType.Room deserializedMap.MapType "Empty map Type"
    
    printfn "--- TileMap Empty/Minimal Map: PASSED ---"

let testTileMapLargeMap () =
    printfn "\n--- Test: TileMap Large Map (100x100) ---"
    
    let voidSprite = SpriteLoc(0, 0, 0)
    let floorSprite = SpriteLoc(1, 1, 1)
    
    let mutable tileProps = TilePropertiesImmutableReference.New("LargeMap")
    let floorProps = {
        Walkable = true
        TileType = TileType.Floor
        Health = 100
        DescriptionKey = "floor"
        Biome = Biome.Plains
        TileOpacity = TileOpacity.Transparent
        DestroyedSpriteLoc = None
        NextStateSpriteLoc = None
    }
    tileProps <- tileProps.Set(floorSprite, floorProps)
    
    let tilePropsRef = TilePropertiesReference()
    tilePropsRef.[floorSprite] <- floorProps
    
    // Create 100x100 grid
    let largeTiles = Array.init 10000 (fun i -> 
        { SpriteLoc = floorSprite; Health = i % 255; IsOccupied = (i % 10) = 0 })
    
    let largeMap = TileMap(100, 100, largeTiles, voidSprite, tilePropsRef, "LargeTest", MapType.Overworld)
    
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let serializedBytes = TileMapSerializer.serialize largeMap
    sw.Stop()
    printfn "Serialized 100x100 map to %d bytes in %dms" serializedBytes.Length sw.ElapsedMilliseconds
    
    sw.Restart()
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    sw.Stop()
    printfn "Deserialized 100x100 map in %dms" sw.ElapsedMilliseconds
    
    assertEquals 100 deserializedMap.Width "Large map Width"
    assertEquals 100 deserializedMap.Height "Large map Height"
    
    // Spot check some tiles
    assertEquals largeTiles.[0].Health (deserializedMap.GetTile(0, 0).Health) "Tile (0,0) Health"
    assertEquals largeTiles.[5050].Health (deserializedMap.GetTile(50, 50).Health) "Tile (50,50) Health"
    assertEquals largeTiles.[9999].Health (deserializedMap.GetTile(99, 99).Health) "Tile (99,99) Health"
    
    printfn "--- TileMap Large Map: PASSED ---"

let testTileMapAllMapTypes () =
    printfn "\n--- Test: TileMap All MapType Values ---"
    
    let voidSprite = SpriteLoc(0, 0, 0)
    let tilePropsRef = TilePropertiesReference()
    let tiles = Array.create 4 { SpriteLoc = voidSprite; Health = 0; IsOccupied = false }
    
    let mapTypes = [
        MapType.Room
        MapType.ComplexBuilding
        MapType.TownOrCity
        MapType.Dungeon
        MapType.Overworld
    ]
    
    for mapType in mapTypes do
        let testMap = TileMap(2, 2, tiles, voidSprite, tilePropsRef, (sprintf "%A" mapType), mapType)
        let serializedBytes = TileMapSerializer.serialize testMap
        let deserializedMap = TileMapSerializer.deserialize serializedBytes
        
        assertEquals mapType deserializedMap.MapType (sprintf "MapType %A" mapType)
    
    printfn "--- TileMap All MapType Values: PASSED ---"

let testTileMapAllBiomes () =
    printfn "\n--- Test: TileMap All Biome Values ---"
    
    let voidSprite = SpriteLoc(0, 0, 0)
    let mutable tileProps = TilePropertiesImmutableReference.New("BiomeTest")
    
    let biomes = [
        Biome.None
        Biome.Forest
        Biome.Desert
        Biome.Snow
        Biome.Swamp
        Biome.Mountain
        Biome.Ocean
        Biome.Plains
    ]
    
    for i, biome in List.indexed biomes do
        let sprite = SpriteLoc(0, i, i)
        let props = {
            Walkable = true
            TileType = TileType.Floor
            Health = 100
            DescriptionKey = sprintf "biome_%A" biome
            Biome = biome
            TileOpacity = TileOpacity.Transparent
            DestroyedSpriteLoc = None
            NextStateSpriteLoc = None
        }
        tileProps <- tileProps.Set(sprite, props)
    
    let tilePropsRef = TilePropertiesReference()
    for KeyValue(spriteLoc, props) in tileProps.Properties do
        tilePropsRef.[spriteLoc] <- props
    
    let tiles = Array.create 1 { SpriteLoc = voidSprite; Health = 0; IsOccupied = false }
    let testMap = TileMap(1, 1, tiles, voidSprite, tilePropsRef, "BiomeTest", MapType.Overworld)
    
    let serializedBytes = TileMapSerializer.serialize testMap
    let deserializedMap = TileMapSerializer.deserialize serializedBytes
    
    for i, biome in List.indexed biomes do
        let sprite = SpriteLoc(0, i, i)
        let deserProps = deserializedMap.TilePropertiesReference.[sprite]
        assertEquals biome deserProps.Biome (sprintf "Biome %A" biome)
    
    printfn "--- TileMap All Biome Values: PASSED ---"

let testTileMapRoundTripMultiple () =
    printfn "\n--- Test: TileMap Multiple Round-Trips ---"
    
    let originalMap = createTestTileMap()
    let mutable currentBytes = TileMapSerializer.serialize originalMap
    let mutable currentMap = originalMap
    
    // Perform 5 round-trips
    for i in 1..5 do
        currentMap <- TileMapSerializer.deserialize currentBytes
        currentBytes <- TileMapSerializer.serialize currentMap
    
    // Final deserialize
    let finalMap = TileMapSerializer.deserialize currentBytes
    
    // Verify data integrity after multiple round-trips
    assertEquals originalMap.Width finalMap.Width "Width after 5 round-trips"
    assertEquals originalMap.Height finalMap.Height "Height after 5 round-trips"
    assertEquals originalMap.MapName finalMap.MapName "MapName after 5 round-trips"
    
    // Check a specific tile
    let origTile = originalMap.GetTile(1, 1)
    let finalTile = finalMap.GetTile(1, 1)
    assertEquals origTile.SpriteLoc finalTile.SpriteLoc "Tile (1,1) after 5 round-trips"
    assertEquals origTile.Health finalTile.Health "Tile (1,1) Health after 5 round-trips"
    
    printfn "--- TileMap Multiple Round-Trips: PASSED ---"


let testEditorTileMapConversion () =
    printfn "\n--- Test: TileMap <-> EditorTileMap Conversion ---"
    
    // Create a test runtime map
    let originalRuntimeMap = createTestTileMap()
    
    // Convert to editor map
    let editorMap = EditorTileMap.FromTileMap(originalRuntimeMap)
    
    // Verify basic properties
    assertEquals originalRuntimeMap.Width editorMap.Width "Width after conversion to EditorTileMap"
    assertEquals originalRuntimeMap.Height editorMap.Height "Height after conversion to EditorTileMap"
    assertEquals originalRuntimeMap.MapName editorMap.MapName "MapName after conversion to EditorTileMap"
    assertEquals originalRuntimeMap.MapType editorMap.MapType "MapType after conversion to EditorTileMap"
    assertEquals originalRuntimeMap.VoidSpriteLoc editorMap.VoidSpriteLoc "VoidSpriteLoc after conversion to EditorTileMap"
    
    // Verify tiles
    for y in 0 .. originalRuntimeMap.Height - 1 do
        for x in 0 .. originalRuntimeMap.Width - 1 do
            let runtimeTile = originalRuntimeMap.GetTile(x, y)
            let editorTile = editorMap.GetTile(x, y)
            assertEquals runtimeTile.SpriteLoc editorTile.SpriteLoc (sprintf "Tile (%d,%d) SpriteLoc" x y)
            assertEquals runtimeTile.Health editorTile.Health (sprintf "Tile (%d,%d) Health" x y)
            assertEquals runtimeTile.IsOccupied editorTile.IsOccupied (sprintf "Tile (%d,%d) IsOccupied" x y)
    
    // Convert back to runtime map
    let convertedRuntimeMap = editorMap.ToTileMap()
    
    // Verify round-trip
    assertEquals originalRuntimeMap.Width convertedRuntimeMap.Width "Width after round-trip"
    assertEquals originalRuntimeMap.Height convertedRuntimeMap.Height "Height after round-trip"
    assertEquals originalRuntimeMap.MapName convertedRuntimeMap.MapName "MapName after round-trip"
    
    for y in 0 .. originalRuntimeMap.Height - 1 do
        for x in 0 .. originalRuntimeMap.Width - 1 do
            let origTile = originalRuntimeMap.GetTile(x, y)
            let convertedTile = convertedRuntimeMap.GetTile(x, y)
            assertEquals origTile.SpriteLoc convertedTile.SpriteLoc (sprintf "Round-trip Tile (%d,%d) SpriteLoc" x y)
            assertEquals origTile.Health convertedTile.Health (sprintf "Round-trip Tile (%d,%d) Health" x y)
    
    printfn "--- TileMap <-> EditorTileMap Conversion: PASSED ---"

let testEditorTileMapEditAndConvert () =
    printfn "\n--- Test: Edit EditorTileMap and Convert Back ---"
    
    let originalRuntimeMap = createTestTileMap()
    let editorMap = EditorTileMap.FromTileMap(originalRuntimeMap)
    
    // Make some edits
    let newTile = { SpriteLoc = SpriteLoc(5, 5, 5); Health = 999; IsOccupied = true }
    let editedMap = editorMap.UpdateTile(1, 1, newTile)
    
    // Convert back
    let convertedMap = editedMap.ToTileMap()
    
    // Verify the edit persisted
    let resultTile = convertedMap.GetTile(1, 1)
    assertEquals newTile.SpriteLoc resultTile.SpriteLoc "Edited tile SpriteLoc"
    assertEquals newTile.Health resultTile.Health "Edited tile Health"
    assertEquals newTile.IsOccupied resultTile.IsOccupied "Edited tile IsOccupied"
    
    printfn "--- Edit EditorTileMap and Convert Back: PASSED ---"

// Run all TileMap serialization tests
let runTests() =
    printfn "\n========== TILEMAP SERIALIZATION TESTS =========="
    testTileMapBasicSerialization ()
    testTileMapTileData ()
    testTileMapPropertiesIntegrity ()
    testTileMapEmptyMap ()
    testTileMapLargeMap ()
    testTileMapAllMapTypes ()
    testTileMapAllBiomes ()
    testTileMapRoundTripMultiple ()
    testEditorTileMapConversion ()
    testEditorTileMapEditAndConvert ()
    printfn "\n========== ALL TILEMAP TESTS PASSED ==========" 

runTests()

