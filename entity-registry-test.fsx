#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"
open AspectGameEngine 
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

//====================== Entity Registry Serialization Tests ======================

let clearRegistries() =
    EntityRegistry.ItemProps.Clear()
    EntityRegistry.FixtureProps.Clear()
    EntityRegistry.ActorProps.Clear()
    EntityRegistry.DecalProps.Clear()

let populateTestRegistries() =
    clearRegistries()
    
    // Add test items
    EntityRegistry.ItemProps.[1001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(0, 1, 1))
        TileOpacity = TileOpacity.Transparent
    }
    
    EntityRegistry.ItemProps.[1002] <- {
        Sprite = SpriteRef.TextureId(5000)
        TileOpacity = TileOpacity.Air
    }
    
    EntityRegistry.ItemProps.[1003] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 2; X = 10; Y = 20; Width = 32; Height = 32 })
        TileOpacity = TileOpacity.Transparent
    }
    
    EntityRegistry.ItemProps.[1004] <- {
        Sprite = SpriteRef.Scene("res://sprites/items/potion.tscn")
        TileOpacity = TileOpacity.Air
    }
    
    // Add test fixtures
    EntityRegistry.FixtureProps.[2001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(1, 5, 5))
        BlocksMovement = true
        Interactable = false
        TileOpacity = TileOpacity.Opaque
    }
    
    EntityRegistry.FixtureProps.[2002] <- {
        Sprite = SpriteRef.TextureId(6000)
        BlocksMovement = false
        Interactable = true
        TileOpacity = TileOpacity.Transparent
    }
    
    EntityRegistry.FixtureProps.[2003] <- {
        Sprite = SpriteRef.Scene("res://fixtures/chest.tscn")
        BlocksMovement = true
        Interactable = true
        TileOpacity = TileOpacity.Opaque
    }
    
    // Add test actors
    EntityRegistry.ActorProps.[3001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(2, 10, 10))
        BlocksMovement = true
        TileOpacity = TileOpacity.Opaque
    }
    
    EntityRegistry.ActorProps.[3002] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 3; X = 64; Y = 64; Width = 48; Height = 48 })
        BlocksMovement = false
        TileOpacity = TileOpacity.Air
    }
    
    // Add test decals
    EntityRegistry.DecalProps.[4001] <- SpriteRef.SheetCell(SpriteSheetCell(4, 2, 3))
    EntityRegistry.DecalProps.[4002] <- SpriteRef.TextureId(7000)
    EntityRegistry.DecalProps.[4003] <- SpriteRef.Scene("res://decals/blood.tscn")

let testBasicSerialization() =
    printfn "\n--- Test: Basic Registry Serialization ---"
    
    populateTestRegistries()
    
    // Serialize
    let bytes = EntityRegistrySerializer.serializeCurrent()
    printfn "Serialized registry to %d bytes" bytes.Length
    
    // Deserialize
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Verify counts
    assertEquals 4 data.Items.Length "Item count"
    assertEquals 3 data.Fixtures.Length "Fixture count"
    assertEquals 2 data.Actors.Length "Actor count"
    assertEquals 3 data.Decals.Length "Decal count"
    
    printfn "--- Basic Registry Serialization: PASSED ---"

let testItemPropertiesIntegrity() =
    printfn "\n--- Test: Item Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Find specific items
    let item1001 = data.Items |> Array.find (fun (id, _) -> id = 1001) |> snd
    let item1002 = data.Items |> Array.find (fun (id, _) -> id = 1002) |> snd
    let item1003 = data.Items |> Array.find (fun (id, _) -> id = 1003) |> snd
    let item1004 = data.Items |> Array.find (fun (id, _) -> id = 1004) |> snd
    
    // Verify SheetCell sprite
    match item1001.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals 0 cell.SheetId "Item 1001 SheetId"
        assertEquals 1 cell.Row "Item 1001 Row"
        assertEquals 1 cell.Column "Item 1001 Column"
    | _ -> failwith "Item 1001 should be SheetCell"
    
    assertEquals TileOpacity.Transparent item1001.TileOpacity "Item 1001 opacity"
    
    // Verify TextureId sprite
    match item1002.Sprite with
    | SpriteRef.TextureId id -> assertEquals 5000 id "Item 1002 TextureId"
    | _ -> failwith "Item 1002 should be TextureId"
    
    assertEquals TileOpacity.Air item1002.TileOpacity "Item 1002 opacity"
    
    // Verify SheetRegion sprite
    match item1003.Sprite with
    | SpriteRef.SheetRegion region ->
        assertEquals 2 region.SheetId "Item 1003 SheetId"
        assertEquals 10 region.X "Item 1003 X"
        assertEquals 20 region.Y "Item 1003 Y"
        assertEquals 32 region.Width "Item 1003 Width"
        assertEquals 32 region.Height "Item 1003 Height"
    | _ -> failwith "Item 1003 should be SheetRegion"
    
    // Verify Scene sprite
    match item1004.Sprite with
    | SpriteRef.Scene path -> assertEquals "res://sprites/items/potion.tscn" path "Item 1004 Scene path"
    | _ -> failwith "Item 1004 should be Scene"
    
    printfn "--- Item Properties Integrity: PASSED ---"

let testFixturePropertiesIntegrity() =
    printfn "\n--- Test: Fixture Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let fixture2001 = data.Fixtures |> Array.find (fun (id, _) -> id = 2001) |> snd
    let fixture2002 = data.Fixtures |> Array.find (fun (id, _) -> id = 2002) |> snd
    let fixture2003 = data.Fixtures |> Array.find (fun (id, _) -> id = 2003) |> snd
    
    // Verify fixture 2001
    match fixture2001.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals 1 cell.SheetId "Fixture 2001 SheetId"
        assertEquals 5 cell.Row "Fixture 2001 Row"
        assertEquals 5 cell.Column "Fixture 2001 Column"
    | _ -> failwith "Fixture 2001 should be SheetCell"
    
    assertTrue fixture2001.BlocksMovement "Fixture 2001 blocks movement"
    assertTrue (not fixture2001.Interactable) "Fixture 2001 not interactable"
    assertEquals TileOpacity.Opaque fixture2001.TileOpacity "Fixture 2001 opacity"
    
    // Verify fixture 2002
    match fixture2002.Sprite with
    | SpriteRef.TextureId id -> assertEquals 6000 id "Fixture 2002 TextureId"
    | _ -> failwith "Fixture 2002 should be TextureId"
    
    assertTrue (not fixture2002.BlocksMovement) "Fixture 2002 doesn't block movement"
    assertTrue fixture2002.Interactable "Fixture 2002 interactable"
    assertEquals TileOpacity.Transparent fixture2002.TileOpacity "Fixture 2002 opacity"
    
    // Verify fixture 2003 (Scene)
    match fixture2003.Sprite with
    | SpriteRef.Scene path -> assertEquals "res://fixtures/chest.tscn" path "Fixture 2003 Scene path"
    | _ -> failwith "Fixture 2003 should be Scene"
    
    assertTrue fixture2003.BlocksMovement "Fixture 2003 blocks movement"
    assertTrue fixture2003.Interactable "Fixture 2003 interactable"
    
    printfn "--- Fixture Properties Integrity: PASSED ---"

let testActorPropertiesIntegrity() =
    printfn "\n--- Test: Actor Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let actor3001 = data.Actors |> Array.find (fun (id, _) -> id = 3001) |> snd
    let actor3002 = data.Actors |> Array.find (fun (id, _) -> id = 3002) |> snd
    
    // Verify actor 3001
    match actor3001.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals 2 cell.SheetId "Actor 3001 SheetId"
        assertEquals 10 cell.Row "Actor 3001 Row"
        assertEquals 10 cell.Column "Actor 3001 Column"
    | _ -> failwith "Actor 3001 should be SheetCell"
    
    assertTrue actor3001.BlocksMovement "Actor 3001 blocks movement"
    assertEquals TileOpacity.Opaque actor3001.TileOpacity "Actor 3001 opacity"
    
    // Verify actor 3002
    match actor3002.Sprite with
    | SpriteRef.SheetRegion region ->
        assertEquals 3 region.SheetId "Actor 3002 SheetId"
        assertEquals 64 region.X "Actor 3002 X"
        assertEquals 64 region.Y "Actor 3002 Y"
        assertEquals 48 region.Width "Actor 3002 Width"
        assertEquals 48 region.Height "Actor 3002 Height"
    | _ -> failwith "Actor 3002 should be SheetRegion"
    
    assertTrue (not actor3002.BlocksMovement) "Actor 3002 doesn't block movement"
    assertEquals TileOpacity.Air actor3002.TileOpacity "Actor 3002 opacity"
    
    printfn "--- Actor Properties Integrity: PASSED ---"

let testDecalPropertiesIntegrity() =
    printfn "\n--- Test: Decal Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let decal4001 = data.Decals |> Array.find (fun (id, _) -> id = 4001) |> snd
    let decal4002 = data.Decals |> Array.find (fun (id, _) -> id = 4002) |> snd
    let decal4003 = data.Decals |> Array.find (fun (id, _) -> id = 4003) |> snd
    
    // Verify decal 4001
    match decal4001 with
    | SpriteRef.SheetCell cell ->
        assertEquals 4 cell.SheetId "Decal 4001 SheetId"
        assertEquals 2 cell.Row "Decal 4001 Row"
        assertEquals 3 cell.Column "Decal 4001 Column"
    | _ -> failwith "Decal 4001 should be SheetCell"
    
    // Verify decal 4002
    match decal4002 with
    | SpriteRef.TextureId id -> assertEquals 7000 id "Decal 4002 TextureId"
    | _ -> failwith "Decal 4002 should be TextureId"
    
    // Verify decal 4003
    match decal4003 with
    | SpriteRef.Scene path -> assertEquals "res://decals/blood.tscn" path "Decal 4003 Scene path"
    | _ -> failwith "Decal 4003 should be Scene"
    
    printfn "--- Decal Properties Integrity: PASSED ---"

let testEmptyRegistries() =
    printfn "\n--- Test: Empty Registries ---"
    
    clearRegistries()
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    assertEquals 0 data.Items.Length "Empty items"
    assertEquals 0 data.Fixtures.Length "Empty fixtures"
    assertEquals 0 data.Actors.Length "Empty actors"
    assertEquals 0 data.Decals.Length "Empty decals"
    
    printfn "--- Empty Registries: PASSED ---"

let testLoadIntoModule() =
    printfn "\n--- Test: Load Into Module ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    
    // Clear registries
    clearRegistries()
    assertEquals 0 EntityRegistry.ItemProps.Count "Items cleared"
    
    // Load back
    EntityRegistrySerializer.loadIntoModule bytes
    
    // Verify loaded data
    assertEquals 4 EntityRegistry.ItemProps.Count "Items loaded"
    assertEquals 3 EntityRegistry.FixtureProps.Count "Fixtures loaded"
    assertEquals 2 EntityRegistry.ActorProps.Count "Actors loaded"
    assertEquals 3 EntityRegistry.DecalProps.Count "Decals loaded"
    
    // Spot check
    assertTrue (EntityRegistry.ItemProps.ContainsKey(1001)) "Item 1001 exists"
    assertTrue (EntityRegistry.FixtureProps.ContainsKey(2002)) "Fixture 2002 exists"
    assertTrue (EntityRegistry.ActorProps.ContainsKey(3001)) "Actor 3001 exists"
    assertTrue (EntityRegistry.DecalProps.ContainsKey(4003)) "Decal 4003 exists"
    
    let item1001 = EntityRegistry.ItemProps.[1001]
    assertEquals TileOpacity.Transparent item1001.TileOpacity "Loaded item opacity"
    
    printfn "--- Load Into Module: PASSED ---"

let testRoundTripMultiple() =
    printfn "\n--- Test: Multiple Round-Trips ---"
    
    populateTestRegistries()
    let mutable bytes = EntityRegistrySerializer.serializeCurrent()
    
    // 5 round-trips
    for i in 1..5 do
        clearRegistries()
        EntityRegistrySerializer.loadIntoModule bytes
        bytes <- EntityRegistrySerializer.serializeCurrent()
    
    // Final deserialize
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Verify integrity after multiple round-trips
    assertEquals 4 data.Items.Length "Items after 5 round-trips"
    assertEquals 3 data.Fixtures.Length "Fixtures after 5 round-trips"
    assertEquals 2 data.Actors.Length "Actors after 5 round-trips"
    assertEquals 3 data.Decals.Length "Decals after 5 round-trips"
    
    // Spot check specific property
    let item1003 = data.Items |> Array.find (fun (id, _) -> id = 1003) |> snd
    match item1003.Sprite with
    | SpriteRef.SheetRegion region ->
        assertEquals 32 region.Width "Item 1003 Width preserved"
        assertEquals 32 region.Height "Item 1003 Height preserved"
    | _ -> failwith "Item 1003 sprite type changed"
    
    printfn "--- Multiple Round-Trips: PASSED ---"

let testAllOpacityValues() =
    printfn "\n--- Test: All TileOpacity Values ---"
    
    clearRegistries()
    
    EntityRegistry.ItemProps.[1] <- {
        Sprite = SpriteRef.TextureId(1)
        TileOpacity = TileOpacity.Opaque
    }
    
    EntityRegistry.ItemProps.[2] <- {
        Sprite = SpriteRef.TextureId(2)
        TileOpacity = TileOpacity.Transparent
    }
    
    EntityRegistry.ItemProps.[3] <- {
        Sprite = SpriteRef.TextureId(3)
        TileOpacity = TileOpacity.Air
    }
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let item1 = data.Items |> Array.find (fun (id, _) -> id = 1) |> snd
    let item2 = data.Items |> Array.find (fun (id, _) -> id = 2) |> snd
    let item3 = data.Items |> Array.find (fun (id, _) -> id = 3) |> snd
    
    assertEquals TileOpacity.Opaque item1.TileOpacity "Opaque preserved"
    assertEquals TileOpacity.Transparent item2.TileOpacity "Transparent preserved"
    assertEquals TileOpacity.Air item3.TileOpacity "Air preserved"
    
    printfn "--- All TileOpacity Values: PASSED ---"

let testLargeRegistry() =
    printfn "\n--- Test: Large Registry (1000 entries each) ---"
    
    clearRegistries()
    
    // Add 1000 items
    for i in 1..1000 do
        EntityRegistry.ItemProps.[i] <- {
            Sprite = SpriteRef.SheetCell(SpriteSheetCell(i % 10, i % 100, i % 100))
            TileOpacity = if i % 3 = 0 then TileOpacity.Opaque elif i % 3 = 1 then TileOpacity.Transparent else TileOpacity.Air
        }
    
    // Add 1000 fixtures
    for i in 1..1000 do
        EntityRegistry.FixtureProps.[i + 1000] <- {
            Sprite = SpriteRef.TextureId(i)
            BlocksMovement = i % 2 = 0
            Interactable = i % 3 = 0
            TileOpacity = if i % 2 = 0 then TileOpacity.Opaque else TileOpacity.Transparent
        }
    
    let sw = Stopwatch.StartNew()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    sw.Stop()
    printfn "Serialized 2000 entries to %d bytes in %dms" bytes.Length sw.ElapsedMilliseconds
    
    sw.Restart()
    let data = EntityRegistrySerializer.deserialize bytes
    sw.Stop()
    printfn "Deserialized in %dms" sw.ElapsedMilliseconds
    
    assertEquals 1000 data.Items.Length "Large item count"
    assertEquals 1000 data.Fixtures.Length "Large fixture count"
    
    // Spot check
    let item500 = data.Items |> Array.find (fun (id, _) -> id = 500) |> snd
    match item500.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals (500 % 10) cell.SheetId "Item 500 SheetId correct"
    | _ -> failwith "Item 500 wrong sprite type"
    
    printfn "--- Large Registry: PASSED ---"

let testAllSpriteRefVariants() =
    printfn "\n--- Test: All SpriteRef Variants ---"
    
    clearRegistries()
    
    // Test each variant in each registry type
    EntityRegistry.ItemProps.[1] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(1, 2, 3))
        TileOpacity = TileOpacity.Air
    }
    
    EntityRegistry.ItemProps.[2] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 5; X = 100; Y = 200; Width = 64; Height = 64 })
        TileOpacity = TileOpacity.Air
    }
    
    EntityRegistry.ItemProps.[3] <- {
        Sprite = SpriteRef.TextureId(9999)
        TileOpacity = TileOpacity.Air
    }
    
    EntityRegistry.ItemProps.[4] <- {
        Sprite = SpriteRef.Scene("res://test/sprite.tscn")
        TileOpacity = TileOpacity.Air
    }
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Verify all variants preserved
    let variants = data.Items |> Array.map (fun (_, props) -> props.Sprite)
    
    let hasSheetCell = variants |> Array.exists (function SpriteRef.SheetCell _ -> true | _ -> false)
    let hasSheetRegion = variants |> Array.exists (function SpriteRef.SheetRegion _ -> true | _ -> false)
    let hasTextureId = variants |> Array.exists (function SpriteRef.TextureId _ -> true | _ -> false)
    let hasScene = variants |> Array.exists (function SpriteRef.Scene _ -> true | _ -> false)
    
    assertTrue hasSheetCell "Has SheetCell variant"
    assertTrue hasSheetRegion "Has SheetRegion variant"
    assertTrue hasTextureId "Has TextureId variant"
    assertTrue hasScene "Has Scene variant"
    
    printfn "--- All SpriteRef Variants: PASSED ---"

let testSerializeFromCustomDictionaries() =
    printfn "\n--- Test: Serialize From Custom Dictionaries ---"
    
    let customItems = System.Collections.Generic.Dictionary<int, ItemProperties>()
    customItems.[100] <- {
        Sprite = SpriteRef.TextureId(1234)
        TileOpacity = TileOpacity.Transparent
    }
    
    let customFixtures = System.Collections.Generic.Dictionary<int, FixtureProperties>()
    let customActors = System.Collections.Generic.Dictionary<int, ActorProperties>()
    let customDecals = System.Collections.Generic.Dictionary<int, SpriteRef>()
    
    let bytes = EntityRegistrySerializer.serializeFrom customItems customFixtures customActors customDecals
    let data = EntityRegistrySerializer.deserialize bytes
    
    assertEquals 1 data.Items.Length "Custom items count"
    assertEquals 0 data.Fixtures.Length "Custom fixtures count"
    
    let item100 = data.Items |> Array.find (fun (id, _) -> id = 100) |> snd
    match item100.Sprite with
    | SpriteRef.TextureId id -> assertEquals 1234 id "Custom item TextureId"
    | _ -> failwith "Wrong sprite type"
    
    printfn "--- Serialize From Custom Dictionaries: PASSED ---"

// Run all tests
let runTests() =
    printfn "\n========== ENTITY REGISTRY SERIALIZATION TESTS =========="
    testBasicSerialization()
    testItemPropertiesIntegrity()
    testFixturePropertiesIntegrity()
    testActorPropertiesIntegrity()
    testDecalPropertiesIntegrity()
    testEmptyRegistries()
    testLoadIntoModule()
    testRoundTripMultiple()
    testAllOpacityValues()
    testLargeRegistry()
    testAllSpriteRefVariants()
    testSerializeFromCustomDictionaries()
    printfn "\n========== ALL ENTITY REGISTRY TESTS PASSED =========="

runTests()