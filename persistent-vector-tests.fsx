#r "./bin/Debug/net8.0/AspectGameEngine.dll"

open System
open System.Collections
open System.Collections.Generic
open FSharpx.Collections

let assertEquals expected actual message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual

let assertTrue condition message =
    if not condition then
        failwithf "ASSERTION FAILED: %s" message

let assertArrayEquals (expected: 'T[]) (actual: 'T[]) message =
    if expected <> actual then
        failwithf "ASSERTION FAILED: %s\nExpected: %A\nActual:   %A" message expected actual

let assertVectorEqualsArray message (expected: int[]) (actual: PersistentVector<int>) =
    assertEquals expected.Length actual.Length (message + " length")
    assertArrayEquals expected (PersistentVector.toArray actual) (message + " toArray")
    if expected.Length > 0 then
        let probes =
            [| 0
               expected.Length / 2
               expected.Length - 1 |]
            |> Array.distinct

        for i in probes do
            assertEquals expected.[i] actual.[i] (sprintf "%s index %d" message i)

type OneShotSeq<'T>(items: 'T[]) =
    let mutable used = false

    interface IEnumerable<'T> with
        member _.GetEnumerator() =
            if used then
                failwith "OneShotSeq was enumerated more than once."
            used <- true
            (items :> seq<'T>).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() =
            (this :> IEnumerable<'T>).GetEnumerator() :> IEnumerator

type BatchRecord = { X: int; Value: int }

let sliceTake n (items: int[]) =
    if n <= 0 then [||]
    elif n >= items.Length then items
    else items.[0 .. n - 1]

let sliceDrop n (items: int[]) =
    if n <= 0 then items
    elif n >= items.Length then [||]
    else items.[n ..]

let replaceAt index value (items: int[]) =
    let copy = Array.copy items
    copy.[index] <- value
    copy

let applyUpdates (updates: (int * int)[]) (items: int[]) =
    let copy = Array.copy items
    for index, value in updates do
        copy.[index] <- value
    copy

let testConstructionAndIndexing () =
    printfn "\n--- PersistentVector construction/indexing ---"

    let sizes =
        [ 0; 1; 2; 31; 32; 33; 63; 64; 65; 1023; 1024; 1025; 32767; 32768; 32769 ]

    for size in sizes do
        let expected = [| 0 .. size - 1 |]
        let vector = PersistentVector.ofSeq expected
        assertVectorEqualsArray (sprintf "ofSeq size %d" size) expected vector

    printfn "construction/indexing passed"

let testConjUpdateAndImmutability () =
    printfn "\n--- PersistentVector conj/update/immutability ---"

    let originalArray = [| 0 .. 64 |]
    let original = PersistentVector.ofSeq originalArray

    let appended = PersistentVector.conj 999 original
    assertVectorEqualsArray "original unchanged after conj" originalArray original
    assertVectorEqualsArray "conj appends" (Array.append originalArray [| 999 |]) appended

    let updated = PersistentVector.update 32 -32 original
    assertVectorEqualsArray "original unchanged after update" originalArray original
    assertVectorEqualsArray "update replaces one value" (replaceAt 32 -32 originalArray) updated

    let appendViaUpdate = PersistentVector.update original.Length 1234 original
    assertVectorEqualsArray "update at Length appends by current contract" (Array.append originalArray [| 1234 |]) appendViaUpdate

    assertTrue (PersistentVector.tryUpdate -1 0 original).IsNone "tryUpdate rejects negative index"
    assertTrue (PersistentVector.tryUpdate (original.Length + 1) 0 original).IsNone "tryUpdate rejects index greater than Length"
    assertTrue (PersistentVector.tryUpdate original.Length 0 original).IsSome "tryUpdate accepts index equal to Length"

    printfn "conj/update/immutability passed"

let testInitialUnconjAndLast () =
    printfn "\n--- PersistentVector initial/unconj/last ---"

    for size in [ 1; 2; 31; 32; 33; 63; 64; 65; 1024; 1025; 1056; 1057 ] do
        let mutable expected = [| 0 .. size - 1 |]
        let mutable vector = PersistentVector.ofSeq expected

        while expected.Length > 0 do
            assertEquals expected.[expected.Length - 1] vector.Last (sprintf "last size %d remaining %d" size expected.Length)
            let initial, last = vector.Unconj
            assertEquals expected.[expected.Length - 1] last (sprintf "unconj last size %d remaining %d" size expected.Length)
            expected <- expected.[0 .. expected.Length - 2]
            vector <- initial
            assertVectorEqualsArray (sprintf "initial size %d remaining %d" size expected.Length) expected vector

        assertTrue vector.TryInitial.IsNone (sprintf "TryInitial empty after size %d" size)
        assertTrue vector.TryLast.IsNone (sprintf "TryLast empty after size %d" size)

    printfn "initial/unconj/last passed"

let testTakeDropAppendConcatAndTransforms () =
    printfn "\n--- PersistentVector take/drop/append/concat/transforms ---"

    let sourceArray = [| 0 .. 1099 |]
    let source = PersistentVector.ofSeq sourceArray
    let cutPoints = [ -3; 0; 1; 31; 32; 33; 500; 1023; 1024; 1099; 1100; 1101 ]

    for n in cutPoints do
        assertVectorEqualsArray (sprintf "take %d" n) (sliceTake n sourceArray) (PersistentVector.take n source)
        assertVectorEqualsArray (sprintf "drop %d" n) (sliceDrop n sourceArray) (PersistentVector.drop n source)
        assertVectorEqualsArray (sprintf "skip %d" n) (sliceDrop n sourceArray) (PersistentVector.skip n source)

    let a = PersistentVector.ofSeq [| 0 .. 40 |]
    let b = PersistentVector.ofSeq [| 41 .. 87 |]
    assertVectorEqualsArray "append vectors" [| 0 .. 87 |] (PersistentVector.append a b)

    let pieces =
        [| PersistentVector.ofSeq [| 0 .. 4 |]
           PersistentVector.empty<int>
           PersistentVector.ofSeq [| 5 .. 9 |] |]

    assertVectorEqualsArray "concat vectors" [| 0 .. 9 |] (PersistentVector.concat pieces)

    let mapped = PersistentVector.map ((*) 2) source
    assertVectorEqualsArray "map" (sourceArray |> Array.map ((*) 2)) mapped

    let mapi = PersistentVector.mapi (fun i v -> i + v) source
    assertVectorEqualsArray "mapi" (sourceArray |> Array.mapi (fun i v -> i + v)) mapi

    let filtered = PersistentVector.filter (fun v -> v % 3 = 0) source
    assertVectorEqualsArray "filter" (sourceArray |> Array.filter (fun v -> v % 3 = 0)) filtered

    let chosen = PersistentVector.choose (fun v -> if v % 100 = 0 then Some(v / 100) else None) source
    assertVectorEqualsArray "choose" (sourceArray |> Array.choose (fun v -> if v % 100 = 0 then Some(v / 100) else None)) chosen

    let reversed = PersistentVector.rev source
    assertVectorEqualsArray "rev" (Array.rev sourceArray) reversed

    let foldSum = PersistentVector.fold (+) 0 source
    assertEquals (Array.sum sourceArray) foldSum "fold sum"

    let foldBackSample =
        source
        |> PersistentVector.take 16
        |> fun v -> PersistentVector.foldBack (fun item acc -> item :: acc) v []

    assertEquals [ 0 .. 15 ] foldBackSample "foldBack order"

    printfn "take/drop/append/concat/transforms passed"

let testBatchUpdatesAndOneShotInputs () =
    printfn "\n--- PersistentVector batch updates and one-shot inputs ---"

    let sourceArray = [| 0 .. 49 |]
    let source = PersistentVector.ofSeq (OneShotSeq sourceArray)
    assertVectorEqualsArray "ofSeq accepts one-shot input" sourceArray source

    let updates = [| 0, 100; 25, 2500; 25, 2600; 49, -49 |]
    let updated = PersistentVector.updateMany (OneShotSeq updates) source
    assertVectorEqualsArray "updateMany accepts one-shot input and keeps last duplicate write" (applyUpdates updates sourceArray) updated

    let indexedUpdates = [| "a", 111; "z", 999 |]
    let indexMap key = if key = "a" then 1 else 48
    let updatedWithIndexMap = PersistentVector.updateManyWithIndexMap indexMap (OneShotSeq indexedUpdates) source
    let expectedIndexMap = applyUpdates [| 1, 111; 48, 999 |] sourceArray
    assertVectorEqualsArray "updateManyWithIndexMap accepts one-shot input" expectedIndexMap updatedWithIndexMap

    let records = [| { X = 2; Value = 222 }; { X = 47; Value = 4747 } |]
    let updatedWith = PersistentVector.updateManyWith (fun r -> r.X, r.Value) (OneShotSeq records) source
    let expectedWith = applyUpdates [| 2, 222; 47, 4747 |] sourceArray
    assertVectorEqualsArray "updateManyWith accepts one-shot input" expectedWith updatedWith

    let vectors =
        OneShotSeq
            [| PersistentVector.ofSeq [| 0; 1 |]
               PersistentVector.empty<int>
               PersistentVector.ofSeq [| 2; 3; 4 |] |]

    assertVectorEqualsArray "concat accepts one-shot vector sequence" [| 0; 1; 2; 3; 4 |] (PersistentVector.concat vectors)

    printfn "batch updates and one-shot inputs passed"

let testRandomizedAgainstArray () =
    printfn "\n--- PersistentVector randomized model check ---"

    let rng = Random 8675309
    let mutable expected = [||]
    let mutable vector = PersistentVector.empty<int>

    for step in 1 .. 2500 do
        let op =
            if expected.Length = 0 then 0
            else rng.Next(0, 8)

        match op with
        | 0 ->
            let value = rng.Next(-10000, 10000)
            expected <- Array.append expected [| value |]
            vector <- PersistentVector.conj value vector

        | 1 ->
            let index = rng.Next expected.Length
            let value = rng.Next(-10000, 10000)
            expected <- replaceAt index value expected
            vector <- PersistentVector.update index value vector

        | 2 ->
            let value = rng.Next(-10000, 10000)
            expected <- Array.append expected [| value |]
            vector <- PersistentVector.update vector.Length value vector

        | 3 ->
            expected <- Array.take (expected.Length - 1) expected
            vector <- PersistentVector.initial vector

        | 4 ->
            let n = rng.Next(-5, expected.Length + 6)
            expected <- sliceTake n expected
            vector <- PersistentVector.take n vector

        | 5 ->
            let n = rng.Next(-5, expected.Length + 6)
            expected <- sliceDrop n expected
            vector <- PersistentVector.drop n vector

        | 6 ->
            let chunk = [| for _ in 1 .. rng.Next(0, 8) -> rng.Next(-10000, 10000) |]
            expected <- Array.append expected chunk
            vector <- PersistentVector.append vector (PersistentVector.ofSeq chunk)

        | _ ->
            let updateCount = rng.Next(1, min 16 expected.Length + 1)
            let updates =
                [| for _ in 1 .. updateCount ->
                    let index = rng.Next expected.Length
                    let value = rng.Next(-10000, 10000)
                    index, value |]

            expected <- applyUpdates updates expected
            vector <- PersistentVector.updateMany updates vector

        assertVectorEqualsArray (sprintf "random step %d" step) expected vector

    printfn "randomized model check passed"

let runAll () =
    printfn "=== Running PersistentVector Correctness Tests ==="
    testConstructionAndIndexing()
    testConjUpdateAndImmutability()
    testInitialUnconjAndLast()
    testTakeDropAppendConcatAndTransforms()
    testBatchUpdatesAndOneShotInputs()
    testRandomizedAgainstArray()
    printfn "=== PersistentVector Correctness Tests Passed ==="

runAll()
