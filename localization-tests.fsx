#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"

open AspectGameEngine
open AspectGameEngine.Localization
open System.Collections.Generic

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let rodict pairs =
    Dictionary(dict pairs) :> Args

let testNegativeExactVariantBinaryRoundTrip () =
    printfn "\n--- Test: Negative exact variant binary round-trip ---"
    let aglText = """
edge.count(count) {
  =-1: "Negative"
  =0: "Zero"
  other: "{count} items"
}
"""
    let pack1 = AglParser.Parse(aglText) |> AglPacker.Build
    let bytes = AglPacker.WriteBinary(pack1)
    let pack2 = AglPacker.ReadBinary(bytes)

    let idx = pack2.Index.["edge.count"]
    match pack2.Messages.[idx] with
    | Plural (_, variants) ->
        match variants.[0].Label with
        | Exact -1 -> ()
        | other -> failwithf "Expected Exact -1, got %A" other
    | other ->
        failwithf "Expected Plural message, got %A" other

    let loc = Localizer(pack2)
    assertEquals "Negative" (loc.Plural("edge.count", -1L, rodict [])) "Exact -1 variant resolves"
    assertEquals "Zero" (loc.Plural("edge.count", 0L, rodict [])) "Exact 0 variant still resolves"
    assertEquals "2 items" (loc.Plural("edge.count", 2L, rodict [])) "Other variant still resolves"

    printfn "--- testNegativeExactVariantBinaryRoundTrip: PASSED ---"

let testEngineMessageLocalizationBridge () =
    printfn "\n--- Test: EngineMessage localization bridge ---"
    let aglText = """
item.rock.name = "Rock"
interaction.take = "Took {count} {itemName}."
"""
    let loc = Localization.loadAgl aglText
    let message: EngineMessage =
        { Key = "interaction.take"
          Args =
            [| "count", EngineMessageArg.Int 3
               "itemName", EngineMessageArg.LocalizedKey "item.rock.name" |] }

    assertEquals "Took 3 Rock." (EngineMessageLocalization.format loc message) "EngineMessage formats typed args"

    let missingMessage: EngineMessage =
        { Key = "interaction.missing"
          Args = [| "itemName", EngineMessageArg.LocalizedKey "item.missing.name" |] }

    assertEquals "interaction.missing" (EngineMessageLocalization.format loc missingMessage) "Missing message key remains visible"
    match EngineMessageLocalization.toArgs loc missingMessage with
    | args ->
        assertEquals true (args.ContainsKey "itemName") "Converted args include localized-key placeholder"
        assertEquals (LocalizedArg.Text "item.missing.name") args.["itemName"] "Missing nested localized key falls back to key"

    printfn "--- testEngineMessageLocalizationBridge: PASSED ---"

testNegativeExactVariantBinaryRoundTrip ()
testEngineMessageLocalizationBridge ()
