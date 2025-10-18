namespace AspectGameEngine

open System
open System.Collections.Generic

[<Struct>]
type RectI = { X:int; Y:int; W:int; H:int }

[<Sealed>]
type ChunkOcclusionManager(width:int, height:int, chunkSize:int) =
    do
        if width <= 0 || height <= 0 || chunkSize <= 0 then invalidArg "dims" "Positive dimensions required."

    // Grid
    let opaque = Array.zeroCreate<bool> (width * height)
    // Doors are excluded from merged geometry; they get dedicated 1x1 occluders when closed.
    let isDoorCell = Array.zeroCreate<bool> (width * height)

    // Chunks
    let chunksX = (width  + chunkSize - 1) / chunkSize
    let chunksY = (height + chunkSize - 1) / chunkSize
    let chunkCount = chunksX * chunksY

    let rectsPerChunk = Array.init chunkCount (fun _ -> ResizeArray<RectI>(16))
    let doorRectsPerChunk = Array.init chunkCount (fun _ -> ResizeArray<RectI>(8))
    let dirtyChunks = HashSet<int>()

    member inline private this.idx x y = y * width + x
    
    member inline private this.chunkIndexOf (cx:int) (cy:int) = cy * chunksX + cx
    
    member _.Width  = width
    member _.Height = height
    member _.ChunkSize = chunkSize
    member _.ChunksX = chunksX
    member _.ChunksY = chunksY

    member private this.MarkChunkDirtyAt(x:int, y:int) =
        let inline chunkOfXY (x:int) (y:int) = (x / chunkSize, y / chunkSize)
        let (cx, cy) = chunkOfXY x y
        if cx >= 0 && cy >= 0 && cx < chunksX && cy < chunksY then
            dirtyChunks.Add(this.chunkIndexOf cx cy) |> ignore

    // Expose: set/clear opaque cell; marks chunk dirty.
    member this.SetOpaque(x:int, y:int, value:bool) =
        if x < 0 || x >= width || y < 0 || y >= height then ()
        else
            let i = this.idx x y
            if opaque.[i] <> value then
                opaque.[i] <- value
                this.MarkChunkDirtyAt(x,y)
                
    member this.SetDoorCellState(x:int, y:int, opacity:TileOpacity) =
        if x < 0 || x >= width || y < 0 || y >= height then ()
        else
            let i = this.idx x y
            let mutable changed = false
            if not isDoorCell.[i] then
                isDoorCell.[i] <- true
                changed <- true
            let v = TileOpacity.isOpaque opacity
            if opaque.[i] <> v then
                opaque.[i] <- v
                changed <- true
            if changed then this.MarkChunkDirtyAt(x,y)

    // Convenience: compute from base tile + layer cell (items ignored).
    // Do not overwrite door cells; their opacity is driven by SetDoorCellState.
    member this.SetFromTileAndLayer(x:int, y:int, baseTileOpacity: TileOpacity, layerCell: LayerCell) =
        if x < 0 || x >= width || y < 0 || y >= height then ()
        else
            let i = this.idx x y
            if isDoorCell.[i] then () else
            let eff =
                match LayerQueries.EffectiveTileOpacity(baseTileOpacity, layerCell) with
                | TileOpacity.Opaque -> true
                | _ -> false
            this.SetOpaque(x,y,eff)

    // Clear all data (keeps arrays allocated)
    member _.ClearAll() =
        Array.Fill(opaque, false)
        Array.Fill(isDoorCell, false)
        for ra in rectsPerChunk do ra.Clear()
        for dra in doorRectsPerChunk do dra.Clear()
        dirtyChunks.Clear()

    // Greedy rectangle cover for a chunk:
    // - Merge only opaque non-door cells into large rects.
    // - Add 1x1 rects for closed doors (opaque door cells).
    member private this.RebuildChunk(cx:int, cy:int) =
        let inline idx x y = y * width + x
        let inline chunkBounds (cx:int) (cy:int) =
            let x0 = cx * chunkSize
            let y0 = cy * chunkSize
            let x1 = Math.Min(width,  x0 + chunkSize)
            let y1 = Math.Min(height, y0 + chunkSize)
            x0, y0, x1, y1   
        let x0, y0, x1, y1 = chunkBounds cx cy
        let wC = x1 - x0
        let hC = y1 - y0
        let localVisited = Array.zeroCreate<bool> (wC * hC)
        let inline lidx (x:int) (y:int) = (y - y0) * wC + (x - x0)

        // Reset and rebuild
        let outRects = rectsPerChunk.[this.chunkIndexOf cx cy]
        outRects.Clear()
        let doorRects = doorRectsPerChunk.[this.chunkIndexOf cx cy]
        doorRects.Clear()

        // Only merge opaque non-door cells
        let inline isSolid x y =
            let gi = idx x y
            opaque.[gi] && not isDoorCell.[gi]  // doors are handled separately

        // Pass 1: merge walls/floors etc. (non-door opaque cells)
        for y = y0 to y1 - 1 do
            for x = x0 to x1 - 1 do
                if isSolid x y && not localVisited.[lidx x y] then
                    // Find maximal width
                    let mutable w = 1
                    while x + w < x1 && isSolid (x + w) y && not localVisited.[lidx (x + w) y] do
                        w <- w + 1
                    // Find maximal height while keeping the same width of solid cells
                    let mutable h = 1
                    let mutable canGrow = true
                    while canGrow && (y + h) < y1 do
                        // Check next row across current width
                        let yy = y + h
                        let mutable rowOk = true
                        let mutable xx = x
                        while rowOk && xx < x + w do
                            if not (isSolid xx yy) || localVisited.[lidx xx yy] then
                                rowOk <- false
                            xx <- xx + 1
                        if rowOk then
                            h <- h + 1
                        else
                            canGrow <- false
                    // Mark visited
                    for yy = y to y + h - 1 do
                        for xx = x to x + w - 1 do
                            localVisited.[lidx xx yy] <- true
                    // Emit rectangle in tile space
                    outRects.Add({ X = x; Y = y; W = w; H = h })

        // Pass 2: add 1x1 occluders for closed doors
        for y = y0 to y1 - 1 do
            for x = x0 to x1 - 1 do
                let gi = idx x y
                if isDoorCell.[gi] && opaque.[gi] then
                    doorRects.Add({ X = x; Y = y; W = 1; H = 1 })

    // Rebuild only dirty chunks
    member this.RebuildDirty() =
        if dirtyChunks.Count = 0 then 0 else
        let mutable rebuilt = 0
        for ci in dirtyChunks do
            let cx = ci % chunksX
            let cy = ci / chunksX
            this.RebuildChunk(cx, cy)
            rebuilt <- rebuilt + 1
        dirtyChunks.Clear()
        rebuilt

    // Access rectangles of a chunk (do not mutate)
    member this.GetChunkRects(cx:int, cy:int) : ResizeArray<RectI> =
        rectsPerChunk.[this.chunkIndexOf cx cy]

    member this.GetChunkDoorRects(cx:int, cy:int) : ResizeArray<RectI> =
        doorRectsPerChunk.[this.chunkIndexOf cx cy]

    // Iterate both merged occluders and door occluders
    member this.ForEachRectInView(viewX0:int, viewY0:int, viewX1:int, viewY1:int, action: RectI -> unit) =
        let inline clamp (v:int) lo hi = if v < lo then lo elif v > hi then hi else v
        let c0x = clamp (viewX0 / chunkSize) 0 (chunksX-1)
        let c0y = clamp (viewY0 / chunkSize) 0 (chunksY-1)
        let c1x = clamp ((viewX1) / chunkSize) 0 (chunksX-1)
        let c1y = clamp ((viewY1) / chunkSize) 0 (chunksY-1)
        for cy = c0y to c1y do
            for cx = c0x to c1x do
                let chunkIndex = this.chunkIndexOf cx cy
                let rects = rectsPerChunk.[chunkIndex]
                let doorRects = doorRectsPerChunk.[chunkIndex]
                // Overlap test in tile space
                let inline visit (r:RectI) =
                    let rx0 = r.X
                    let ry0 = r.Y
                    let rx1 = r.X + r.W
                    let ry1 = r.Y + r.H
                    if not (rx1 <= viewX0 || ry1 <= viewY0 || rx0 >= viewX1 || ry0 >= viewY1) then
                        action r
                for r in rects do visit r
                for r in doorRects do visit r