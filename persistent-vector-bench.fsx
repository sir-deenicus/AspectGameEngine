#r "./bin/Debug/net8.0/AspectGameEngine.dll"

open System
open System.Diagnostics
open FSharpx.Collections

let forceFullGc () =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()

let time label action =
    forceFullGc()
    let sw = Stopwatch.StartNew()
    let result = action()
    sw.Stop()
    printfn "%-34s %9.3f ms" label sw.Elapsed.TotalMilliseconds
    result

let checksumVector (vector: PersistentVector<int>) =
    let mutable sum = 0L
    for value in vector do
        sum <- sum + int64 value
    sum

let count = 100_000
let readCount = 200_000
let updateCount = 5_000
let rng = Random 123456

let sourceArray = [| 0 .. count - 1 |]
let readIndices = [| for _ in 1 .. readCount -> rng.Next count |]
let updateIndices = [| for _ in 1 .. updateCount -> rng.Next count |]
let updates = updateIndices |> Array.map (fun i -> i, -i)

printfn "=== PersistentVector Speed Bench ==="
printfn "This is a deterministic local benchmark, not a pass/fail gate."
printfn "count=%d readCount=%d updateCount=%d" count readCount updateCount
printfn ""

let vector =
    time "ofSeq 100k" (fun () ->
        PersistentVector.ofSeq sourceArray)

let arrayCopy =
    time "Array.copy 100k baseline" (fun () ->
        Array.copy sourceArray)

let vectorReadSum =
    time "vector random reads 200k" (fun () ->
        let mutable sum = 0L
        for index in readIndices do
            sum <- sum + int64 vector.[index]
        sum)

let arrayReadSum =
    time "array random reads 200k baseline" (fun () ->
        let mutable sum = 0L
        for index in readIndices do
            sum <- sum + int64 arrayCopy.[index]
        sum)

let singleUpdated =
    time "persistent Update x5000" (fun () ->
        let mutable v = vector
        for index, value in updates do
            v <- PersistentVector.update index value v
        v)

let batchUpdated =
    time "updateMany x5000" (fun () ->
        PersistentVector.updateMany updates vector)

let appended =
    time "append 100k + 100k" (fun () ->
        PersistentVector.append vector vector)

let concatenated =
    let rows =
        [| for r in 0 .. 999 ->
            PersistentVector.init 100 (fun c -> r * 100 + c) |]

    time "concat 1000x100" (fun () ->
        PersistentVector.concat rows)

let copiedToArray =
    time "toArray 100k" (fun () ->
        PersistentVector.toArray vector)

printfn ""
printfn "checksums:"
printfn "vectorReadSum=%d arrayReadSum=%d" vectorReadSum arrayReadSum
printfn "singleUpdated=%d batchUpdated=%d appended=%d concatenated=%d copiedToArray=%d"
    (checksumVector singleUpdated)
    (checksumVector batchUpdated)
    (checksumVector appended)
    (checksumVector concatenated)
    (Array.sumBy int64 copiedToArray)

printfn "=== PersistentVector Speed Bench Complete ==="
