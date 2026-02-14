#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"
open AspectGameEngine 
open System
open System.Diagnostics
open System.Collections.Generic

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
    EntityRegistry.SpriteProps.Clear()

let populateTestRegistries() =
    clearRegistries()
    
    // Add test items
    EntityRegistry.SpriteProps.[1001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(0, 1, 1))
        SpriteType = SpriteType.Item { DescKey = "item_sword" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[1002] <- {
        Sprite = SpriteRef.TextureId(5000)
        SpriteType = SpriteType.Item { DescKey = "item_shield" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[1003] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 2; X = 10; Y = 20; Width = 32; Height = 32 })
        SpriteType = SpriteType.Item { DescKey = "item_helmet" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[1004] <- {
        Sprite = SpriteRef.Scene("res://sprites/items/potion.tscn")
        SpriteType = SpriteType.Item { DescKey = "item_potion" }
        RenderLayer = 1
    }
    
    // Add test fixtures
    EntityRegistry.SpriteProps.[2001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(1, 5, 5))
        SpriteType = SpriteType.Fixture {
            BlocksMovement = true
            Interactable = false
            Moveable = 0
            DescKey = "fixture_stone_wall"
            TileOpacity = TileOpacity.Opaque
        }
        RenderLayer = 10
    }
    
    EntityRegistry.SpriteProps.[2002] <- {
        Sprite = SpriteRef.TextureId(6000)
        SpriteType = SpriteType.Fixture {
            BlocksMovement = false
            Interactable = true
            Moveable = 1
            DescKey = "fixture_lever"
            TileOpacity = TileOpacity.Transparent
        }
        RenderLayer = 10
    }
    
    EntityRegistry.SpriteProps.[2003] <- {
        Sprite = SpriteRef.Scene("res://fixtures/chest.tscn")
        SpriteType = SpriteType.Fixture {
            BlocksMovement = true
            Interactable = true
            Moveable = 0
            DescKey = "fixture_chest"
            TileOpacity = TileOpacity.Opaque
        }
        RenderLayer = 10
    }
    
    // Add test actors
    EntityRegistry.SpriteProps.[3001] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(2, 10, 10))
        SpriteType = SpriteType.Actor {
            TileOpacity = TileOpacity.Opaque
            DescKey = "actor_goblin"
        }
        RenderLayer = 50
    }
    
    EntityRegistry.SpriteProps.[3002] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 3; X = 64; Y = 64; Width = 48; Height = 48 })
        SpriteType = SpriteType.Actor {
            TileOpacity = TileOpacity.Air
            DescKey = "actor_ghost"
        }
        RenderLayer = 50
    }

    EntityRegistry.SpriteProps.[3003] <- {
        Sprite = SpriteRef.TextureId(8000)
        SpriteType = SpriteType.Actor {
            TileOpacity = TileOpacity.Translucent
            DescKey = "actor_slime"
        }
        RenderLayer = 50
    }
    
    // Add test decals
    EntityRegistry.SpriteProps.[4001] <- { Sprite = SpriteRef.SheetCell(SpriteSheetCell(4, 2, 3)); SpriteType = SpriteType.Decal { Interactable = false; DescKey = "decal_stain" }; RenderLayer = 1000 }
    EntityRegistry.SpriteProps.[4002] <- { Sprite = SpriteRef.TextureId(7000); SpriteType = SpriteType.Decal { Interactable = true; DescKey = "decal_mark" }; RenderLayer = 1000 }
    EntityRegistry.SpriteProps.[4003] <- { Sprite = SpriteRef.Scene("res://decals/blood.tscn"); SpriteType = SpriteType.Decal { Interactable = false; DescKey = "decal_blood" }; RenderLayer = 1000 }

let testBasicSerialization() =
    printfn "\n--- Test: Basic Registry Serialization ---"
    
    populateTestRegistries()
    
    // Serialize
    let bytes = EntityRegistrySerializer.serializeCurrent()
    printfn "Serialized registry to %d bytes" bytes.Length
    
    // Deserialize
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Verify total count
    assertEquals 13 data.SpriteProps.Length "Total sprite props count"
    
    // Verify individual counts by filtering
    let items = data.SpriteProps |> Array.filter (fun (_, sp) -> match sp.SpriteType with SpriteType.Item _ -> true | _ -> false)
    let fixtures = data.SpriteProps |> Array.filter (fun (_, sp) -> match sp.SpriteType with SpriteType.Fixture _ -> true | _ -> false)
    let actors = data.SpriteProps |> Array.filter (fun (_, sp) -> match sp.SpriteType with SpriteType.Actor _ -> true | _ -> false)
    let decals = data.SpriteProps |> Array.filter (fun (_, sp) -> match sp.SpriteType with SpriteType.Decal _ -> true | _ -> false)

    assertEquals 4 items.Length "Item count"
    assertEquals 3 fixtures.Length "Fixture count"
    assertEquals 3 actors.Length "Actor count"
    assertEquals 3 decals.Length "Decal count"

    // Spot-check RenderLayer round-trip
    let sp1001 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1001) |> snd
    assertEquals 1 sp1001.RenderLayer "RenderLayer for item 1001 preserved"
    let sp4001 = data.SpriteProps |> Array.find (fun (id, _) -> id = 4001) |> snd
    assertEquals 1000 sp4001.RenderLayer "RenderLayer for decal 4001 preserved"
    
    printfn "--- Basic Registry Serialization: PASSED ---"

let testItemPropertiesIntegrity() =
    printfn "\n--- Test: Item Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Find specific items
    let item1001 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1001) |> snd
    let item1002 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1002) |> snd
    let item1003 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1003) |> snd
    let item1004 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1004) |> snd

    match item1001.SpriteType with
    | SpriteType.Item ip ->
        assertEquals "item_sword" ip.DescKey "Item 1001 DescKey" 
    | _ -> failwith "Item 1001 should be Item type"
    
    // Verify SheetCell sprite
    match item1001.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals 0 cell.SheetId "Item 1001 SheetId"
        assertEquals 1 cell.Row "Item 1001 Row"
        assertEquals 1 cell.Column "Item 1001 Column"
    | _ -> failwith "Item 1001 should be SheetCell"
    
    // Verify TextureId sprite
    match item1002.Sprite with
    | SpriteRef.TextureId id -> assertEquals 5000 id "Item 1002 TextureId"
    | _ -> failwith "Item 1002 should be TextureId"
    
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
    
    let getFixtureProps id =
        let sp = data.SpriteProps |> Array.find (fun (fid, _) -> fid = id) |> snd
        match sp.SpriteType with
        | SpriteType.Fixture fp -> sp.Sprite, fp
        | _ -> failwithf "Entity %d is not a fixture" id

    let s1, f1 = getFixtureProps 2001
    let s2, f2 = getFixtureProps 2002
    let s3, f3 = getFixtureProps 2003
    
    // Verify fixture 2001
    match s1 with
    | SpriteRef.SheetCell cell ->
        assertEquals 1 cell.SheetId "Fixture 2001 SheetId"
        assertEquals 5 cell.Row "Fixture 2001 Row"
        assertEquals 5 cell.Column "Fixture 2001 Column"
    | _ -> failwith "Fixture 2001 should be SheetCell"
    
    assertTrue f1.BlocksMovement "Fixture 2001 blocks movement"
    assertTrue (not f1.Interactable) "Fixture 2001 not interactable"
    assertEquals "fixture_stone_wall" f1.DescKey "Fixture 2001 DescKey"
    assertEquals TileOpacity.Opaque f1.TileOpacity "Fixture 2001 opacity"
    
    // Verify fixture 2002
    match s2 with
    | SpriteRef.TextureId id -> assertEquals 6000 id "Fixture 2002 TextureId"
    | _ -> failwith "Fixture 2002 should be TextureId"
    
    assertTrue (not f2.BlocksMovement) "Fixture 2002 doesn't block movement"
    assertTrue f2.Interactable "Fixture 2002 interactable"
    assertEquals "fixture_lever" f2.DescKey "Fixture 2002 DescKey"
    assertEquals TileOpacity.Transparent f2.TileOpacity "Fixture 2002 opacity"
    
    // Verify fixture 2003 (Scene)
    match s3 with
    | SpriteRef.Scene path -> assertEquals "res://fixtures/chest.tscn" path "Fixture 2003 Scene path"
    | _ -> failwith "Fixture 2003 should be Scene"
    
    assertTrue f3.BlocksMovement "Fixture 2003 blocks movement"
    assertTrue f3.Interactable "Fixture 2003 interactable"
    assertEquals "fixture_chest" f3.DescKey "Fixture 2003 DescKey"
    
    printfn "--- Fixture Properties Integrity: PASSED ---"

let testActorPropertiesIntegrity() =
    printfn "\n--- Test: Actor Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let getActorProps id =
        let sp = data.SpriteProps |> Array.find (fun (aid, _) -> aid = id) |> snd
        match sp.SpriteType with
        | SpriteType.Actor ap -> sp.Sprite, ap
        | _ -> failwithf "Entity %d is not an actor" id

    let s1, a1 = getActorProps 3001
    let s2, a2 = getActorProps 3002
    let s3, a3 = getActorProps 3003
    
    // Verify actor 3001
    match s1 with
    | SpriteRef.SheetCell cell ->
        assertEquals 2 cell.SheetId "Actor 3001 SheetId"
        assertEquals 10 cell.Row "Actor 3001 Row"
        assertEquals 10 cell.Column "Actor 3001 Column"
    | _ -> failwith "Actor 3001 should be SheetCell"
    
    assertEquals TileOpacity.Opaque a1.TileOpacity "Actor 3001 opacity"
    assertEquals "actor_goblin" a1.DescKey "Actor 3001 DescKey"
    
    // Verify actor 3002
    match s2 with
    | SpriteRef.SheetRegion region ->
        assertEquals 3 region.SheetId "Actor 3002 SheetId"
        assertEquals 64 region.X "Actor 3002 X"
        assertEquals 64 region.Y "Actor 3002 Y"
        assertEquals 48 region.Width "Actor 3002 Width"
        assertEquals 48 region.Height "Actor 3002 Height"
    | _ -> failwith "Actor 3002 should be SheetRegion"
    
    assertEquals TileOpacity.Air a2.TileOpacity "Actor 3002 opacity"
    assertEquals "actor_ghost" a2.DescKey "Actor 3002 DescKey"

    // Verify actor 3003
    match s3 with
    | SpriteRef.TextureId id -> assertEquals 8000 id "Actor 3003 TextureId"
    | _ -> failwith "Actor 3003 should be TextureId"
    assertEquals TileOpacity.Translucent a3.TileOpacity "Actor 3003 opacity"
    assertEquals "actor_slime" a3.DescKey "Actor 3003 DescKey"
    
    printfn "--- Actor Properties Integrity: PASSED ---"

let testDecalPropertiesIntegrity() =
    printfn "\n--- Test: Decal Properties Integrity ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let getDecalProps id =
        let sp = data.SpriteProps |> Array.find (fun (did, _) -> did = id) |> snd
        match sp.SpriteType with
        | SpriteType.Decal dp -> sp.Sprite, dp
        | _ -> failwithf "Entity %d is not a decal" id

    let decal4001, dp1 = getDecalProps 4001
    let decal4002, dp2 = getDecalProps 4002
    let decal4003, dp3 = getDecalProps 4003
    
    // Verify decal 4001
    match decal4001 with
    | SpriteRef.SheetCell cell ->
        assertEquals 4 cell.SheetId "Decal 4001 SheetId"
        assertEquals 2 cell.Row "Decal 4001 Row"
        assertEquals 3 cell.Column "Decal 4001 Column"
    | _ -> failwith "Decal 4001 should be SheetCell"
    assertTrue (not dp1.Interactable) "Decal 4001 not interactable"
    assertEquals "decal_stain" dp1.DescKey "Decal 4001 DescKey"
    
    // Verify decal 4002
    match decal4002 with
    | SpriteRef.TextureId id -> assertEquals 7000 id "Decal 4002 TextureId"
    | _ -> failwith "Decal 4002 should be TextureId"
    assertTrue dp2.Interactable "Decal 4002 interactable"
    assertEquals "decal_mark" dp2.DescKey "Decal 4002 DescKey"
    
    // Verify decal 4003
    match decal4003 with
    | SpriteRef.Scene path -> assertEquals "res://decals/blood.tscn" path "Decal 4003 Scene path"
    | _ -> failwith "Decal 4003 should be Scene"
    assertTrue (not dp3.Interactable) "Decal 4003 not interactable"
    assertEquals "decal_blood" dp3.DescKey "Decal 4003 DescKey"
    
    printfn "--- Decal Properties Integrity: PASSED ---"

let testEmptyRegistries() =
    printfn "\n--- Test: Empty Registries ---"
    
    clearRegistries()
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    assertEquals 0 data.SpriteProps.Length "Empty sprite props"
    
    printfn "--- Empty Registries: PASSED ---"

let testLoadIntoModule() =
    printfn "\n--- Test: Load Into Module ---"
    
    populateTestRegistries()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    
    // Clear registries
    clearRegistries()
    assertEquals 0 EntityRegistry.SpriteProps.Count "Registries cleared"
    
    // Load back
    EntityRegistrySerializer.loadIntoModule bytes
    
    // Verify loaded data
    assertEquals 13 EntityRegistry.SpriteProps.Count "SpriteProps loaded"
    
    // Spot check
    assertTrue (EntityRegistry.SpriteProps.ContainsKey(1001)) "Item 1001 exists"
    assertTrue (EntityRegistry.SpriteProps.ContainsKey(2002)) "Fixture 2002 exists"
    assertTrue (EntityRegistry.SpriteProps.ContainsKey(3001)) "Actor 3001 exists"
    assertTrue (EntityRegistry.SpriteProps.ContainsKey(4003)) "Decal 4003 exists"
    
    let sp1001 = EntityRegistry.SpriteProps.[1001]
    match sp1001.SpriteType with
    | SpriteType.Item _ -> ()
    | _ -> failwith "Item 1001 has wrong type"
    
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
    assertEquals 13 data.SpriteProps.Length "SpriteProps after 5 round-trips"
    
    // Spot check specific property
    let item1003 = data.SpriteProps |> Array.find (fun (id, _) -> id = 1003) |> snd
    match item1003.Sprite with
    | SpriteRef.SheetRegion region ->
        assertEquals 32 region.Width "Item 1003 Width preserved"
        assertEquals 32 region.Height "Item 1003 Height preserved"
    | _ -> failwith "Item 1003 sprite type changed"
    
    printfn "--- Multiple Round-Trips: PASSED ---"

let testAllOpacityValues() =
    printfn "\n--- Test: All TileOpacity Values ---"
    
    clearRegistries()
    
    EntityRegistry.SpriteProps.[1] <- {
        Sprite = SpriteRef.TextureId(1)
        SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; Moveable = 0; DescKey = ""; TileOpacity = TileOpacity.Opaque }
        RenderLayer = 10
    }
    
    EntityRegistry.SpriteProps.[2] <- {
        Sprite = SpriteRef.TextureId(2)
        SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; Moveable = 0; DescKey = ""; TileOpacity = TileOpacity.Transparent }
        RenderLayer = 10
    }
    
    EntityRegistry.SpriteProps.[3] <- {
        Sprite = SpriteRef.TextureId(3)
        SpriteType = SpriteType.Fixture { BlocksMovement = true; Interactable = false; Moveable = 0; DescKey = ""; TileOpacity = TileOpacity.Air }
        RenderLayer = 10
    }
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    let getOpacity id =
        match (data.SpriteProps |> Array.find (fun (fid, _) -> fid = id) |> snd).SpriteType with
        | SpriteType.Fixture fp -> fp.TileOpacity
        | _ -> failwith "Not a fixture"

    assertEquals TileOpacity.Opaque (getOpacity 1) "Opaque preserved"
    assertEquals TileOpacity.Transparent (getOpacity 2) "Transparent preserved"
    assertEquals TileOpacity.Air (getOpacity 3) "Air preserved"
    
    printfn "--- All TileOpacity Values: PASSED ---"

let testLargeRegistry() =
    printfn "\n--- Test: Large Registry (2000 entries) ---"
    
    clearRegistries()
    
    for i in 1..1000 do
        EntityRegistry.SpriteProps.[i] <- {
            Sprite = SpriteRef.SheetCell(SpriteSheetCell(i % 10, i % 100, i % 100))
            SpriteType = SpriteType.Item { DescKey = "" }
            RenderLayer = 1
        }
    
    for i in 1001..2000 do
        EntityRegistry.SpriteProps.[i] <- {
            Sprite = SpriteRef.TextureId(i)
            SpriteType = SpriteType.Fixture {
                BlocksMovement = i % 2 = 0
                Interactable = i % 3 = 0
                Moveable = 0
                DescKey = sprintf "fixture_%d" i
                TileOpacity = if i % 2 = 0 then TileOpacity.Opaque else TileOpacity.Transparent
            }
            RenderLayer = 10
        }
    
    let sw = Stopwatch.StartNew()
    let bytes = EntityRegistrySerializer.serializeCurrent()
    sw.Stop()
    printfn "Serialized 2000 entries to %d bytes in %dms" bytes.Length sw.ElapsedMilliseconds
    
    sw.Restart()
    let data = EntityRegistrySerializer.deserialize bytes
    sw.Stop()
    printfn "Deserialized in %dms" sw.ElapsedMilliseconds
    
    assertEquals 2000 data.SpriteProps.Length "Large count"
    
    // Spot check
    let item500 = data.SpriteProps |> Array.find (fun (id, _) -> id = 500) |> snd
    match item500.Sprite with
    | SpriteRef.SheetCell cell ->
        assertEquals (500 % 10) cell.SheetId "Item 500 SheetId correct"
    | _ -> failwith "Item 500 wrong sprite type"
    
    printfn "--- Large Registry: PASSED ---"

let testAllSpriteRefVariants() =
    printfn "\n--- Test: All SpriteRef Variants ---"
    
    clearRegistries()
    
    // Test each variant
    EntityRegistry.SpriteProps.[1] <- {
        Sprite = SpriteRef.SheetCell(SpriteSheetCell(1, 2, 3))
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[2] <- {
        Sprite = SpriteRef.SheetRegion({ SheetId = 5; X = 100; Y = 200; Width = 64; Height = 64 })
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[3] <- {
        Sprite = SpriteRef.TextureId(9999)
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }
    
    EntityRegistry.SpriteProps.[4] <- {
        Sprite = SpriteRef.Scene("res://test/sprite.tscn")
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }

    EntityRegistry.SpriteProps.[5] <- {
        Sprite = SpriteRef.SheetSpan({ TopLeft = SpriteSheetCell(1, 0, 0); WidthCells = 2; HeightCells = 2 })
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }

    EntityRegistry.SpriteProps.[6] <- {
        Sprite = SpriteRef.SheetCells([| SpriteSheetCell(1, 0, 0); SpriteSheetCell(1, 0, 1) |])
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }
    
    let bytes = EntityRegistrySerializer.serializeCurrent()
    let data = EntityRegistrySerializer.deserialize bytes
    
    // Verify all variants preserved
    let variants = data.SpriteProps |> Array.map (fun (_, props) -> props.Sprite)
    
    let hasSheetCell = variants |> Array.exists (function SpriteRef.SheetCell _ -> true | _ -> false)
    let hasSheetRegion = variants |> Array.exists (function SpriteRef.SheetRegion _ -> true | _ -> false)
    let hasTextureId = variants |> Array.exists (function SpriteRef.TextureId _ -> true | _ -> false)
    let hasScene = variants |> Array.exists (function SpriteRef.Scene _ -> true | _ -> false)
    let hasSheetSpan = variants |> Array.exists (function SpriteRef.SheetSpan _ -> true | _ -> false)
    let hasSheetCells = variants |> Array.exists (function SpriteRef.SheetCells _ -> true | _ -> false)
    
    assertTrue hasSheetCell "Has SheetCell variant"
    assertTrue hasSheetRegion "Has SheetRegion variant"
    assertTrue hasTextureId "Has TextureId variant"
    assertTrue hasScene "Has Scene variant"
    assertTrue hasSheetSpan "Has SheetSpan variant"
    assertTrue hasSheetCells "Has SheetCells variant"

    // Spot check SheetSpan
    let span = variants |> Array.pick (function SpriteRef.SheetSpan s -> Some s | _ -> None)
    assertEquals 1 span.TopLeft.SheetId "SheetSpan SheetId"
    assertEquals 2 span.WidthCells "SheetSpan Width"

    // Spot check SheetCells
    let cells = variants |> Array.pick (function SpriteRef.SheetCells c -> Some c | _ -> None)
    assertEquals 2 cells.Length "SheetCells Length"
    assertEquals 1 cells.[1].Column "SheetCells[1] Column"
    assertEquals (SpriteSheetCell(1, 0, 1)) cells.[1] "SheetCells[1] integrity"
   
    printfn "--- All SpriteRef Variants: PASSED ---"

let testSerializeFromCustomDictionary() =
    printfn "\n--- Test: Serialize From Custom Dictionary ---"
    
    let customProps = Dictionary<int, SpriteProperties>()
    customProps.[100] <- {
        Sprite = SpriteRef.TextureId(1234)
        SpriteType = SpriteType.Item { DescKey = "" }
        RenderLayer = 1
    }
    
    let bytes = EntityRegistrySerializer.serializeFrom customProps
    let data = EntityRegistrySerializer.deserialize bytes
    
    assertEquals 1 data.SpriteProps.Length "Custom items count"
    
    let item100 = data.SpriteProps |> Array.find (fun (id, _) -> id = 100) |> snd
    match item100.Sprite with
    | SpriteRef.TextureId id -> assertEquals 1234 id "Custom item TextureId"
    | _ -> failwith "Wrong sprite type"
    
    printfn "--- Serialize From Custom Dictionary: PASSED ---"

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
    testSerializeFromCustomDictionary()
    printfn "\n========== ALL ENTITY REGISTRY TESTS PASSED =========="

runTests()