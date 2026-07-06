#r "nuget: Google.FlatBuffers, 25.2.10"
#r "bin/Debug/net8.0/AspectGameEngine.dll"

open AspectGameEngine.Localization
open System.Collections.Generic

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual
    else
        printfn "PASSED: %s (Value: %A)" message actual

let rodict pairs =
    Dictionary(dict pairs) :> IReadOnlyDictionary<string, obj>

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

testNegativeExactVariantBinaryRoundTrip ()
