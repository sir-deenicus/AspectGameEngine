namespace AspectGameEngine

open System
open System.Runtime.CompilerServices

[<Struct>]
type internal FovSlope =
    { Num: int
      Den: int }

[<Struct>]
type internal FovInterval =
    { Start: FovSlope
      End: FovSlope
      Cost: byte }

module private Slope =
    let zero = { Num = 0; Den = 1 }
    let one = { Num = 1; Den = 1 }

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline cmp (a: FovSlope) (b: FovSlope) =
        compare (int64 a.Num * int64 b.Den) (int64 b.Num * int64 a.Den)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline lt a b = cmp a b < 0

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline min a b = if cmp a b <= 0 then a else b

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline max a b = if cmp a b >= 0 then a else b

type VisibilityState(width: int, height: int) =
    let visibleStamp = Array.zeroCreate<int> (width * height)
    let translucencyCost = Array.create<byte> (width * height) 255uy
    let intervalCapacity = max 8 ((width * height) + 8)
    let intervalBufferA = Array.zeroCreate<FovInterval> intervalCapacity
    let intervalBufferB = Array.zeroCreate<FovInterval> intervalCapacity
    let mutable tick = 1

    member _.Width = width
    member _.Height = height

    member _.BeginStep() =
        if tick = Int32.MaxValue then
            Array.Clear(visibleStamp, 0, visibleStamp.Length)
            tick <- 1
        else
            tick <- tick + 1

    member _.IsVisible(index: int) =
        visibleStamp.[index] = tick

    member _.GetTranslucencyCost(index: int) =
        if visibleStamp.[index] = tick then int translucencyCost.[index]
        else -1

    member internal _.VisibleStampArray = visibleStamp
    member internal _.TranslucencyCostArray = translucencyCost
    member internal _.IntervalBufferA = intervalBufferA
    member internal _.IntervalBufferB = intervalBufferB
    member internal _.CurrentTick = tick

module RectFov =
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private index (width: int) (x: int) (y: int) = y * width + x

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private tileSpan (depth: int) (minor: int) =
        let lower =
            if minor = 0 then Slope.zero
            else
                { Num = (2 * minor) - 1
                  Den = (2 * depth) + 1 }

        let upper =
            if minor = depth then Slope.one
            else
                { Num = (2 * minor) + 1
                  Den = (2 * depth) - 1 }

        struct (lower, upper)

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private stampVisible
        (map: TileMap)
        (mapWidth: int)
        (x: int)
        (y: int)
        (cost: byte)
        (currentTick: int)
        (visibleStamp: int[])
        (translucencyCost: byte[])
        =
        let idx = index mapWidth x y

        if visibleStamp.[idx] <> currentTick then
            visibleStamp.[idx] <- currentTick
            translucencyCost.[idx] <- cost
        elif cost < translucencyCost.[idx] then
            translucencyCost.[idx] <- cost

        if cost = 0uy then
            map.Explored.[idx] <- 1uy

    let private minOverlappingCostFrom
        (intervals: FovInterval[])
        (intervalCount: int)
        (spanStart: FovSlope)
        (spanEnd: FovSlope)
        (startIndex: int)
        =
        let mutable best = Int32.MaxValue
        let mutable i = startIndex

        while i < intervalCount && Slope.cmp intervals.[i].End spanStart <= 0 do
            i <- i + 1

        let nextStartIndex = i

        while i < intervalCount do
            let interval = intervals.[i]

            if Slope.cmp interval.Start spanEnd >= 0 then
                i <- intervalCount
            else
                let cost = int interval.Cost
                if cost < best then
                    best <- cost

                i <- i + 1

        struct (best, nextStartIndex)

    let private applyOccluderSpan
        (active: FovInterval[])
        (activeCount: int)
        (scratch: FovInterval[])
        (spanStart: FovSlope)
        (spanEnd: FovSlope)
        (opacity: TileOpacity)
        (budget: int)
        =
        let mutable outCount = 0

        let inline append startSlope endSlope cost =
            if Slope.lt startSlope endSlope then
                if outCount > 0
                   && scratch.[outCount - 1].Cost = cost
                   && Slope.cmp scratch.[outCount - 1].End startSlope >= 0 then
                    let previous = scratch.[outCount - 1]
                    scratch.[outCount - 1] <- { previous with End = Slope.max previous.End endSlope }
                else
                    if outCount >= scratch.Length then
                        failwith "Visibility interval buffer capacity exceeded."

                    scratch.[outCount] <-
                        { Start = startSlope
                          End = endSlope
                          Cost = cost }

                    outCount <- outCount + 1

        for i in 0 .. activeCount - 1 do
            let interval = active.[i]

            if Slope.cmp interval.End spanStart <= 0 || Slope.cmp interval.Start spanEnd >= 0 then
                append interval.Start interval.End interval.Cost
            else
                let overlapStart = Slope.max interval.Start spanStart
                let overlapEnd = Slope.min interval.End spanEnd

                match opacity with
                | TileOpacity.Translucent ->
                    append interval.Start overlapStart interval.Cost

                    let nextCost = (int interval.Cost) + 1
                    if nextCost <= budget then
                        append overlapStart overlapEnd (byte nextCost)

                    append overlapEnd interval.End interval.Cost
                | _ ->
                    append interval.Start overlapStart interval.Cost
                    append overlapEnd interval.End interval.Cost

        outCount

    let inline halfTilesForViewport (viewportPixels: int) (tilePixels: int) (overscanTiles: int) =
        let tiles =
            if viewportPixels <= 0 || tilePixels <= 0 then 0
            else (viewportPixels + tilePixels - 1) / tilePixels

        let overscan = if overscanTiles < 0 then 0 else overscanTiles
        (tiles / 2) + overscan

    let compute
        (map: TileMap)
        (state: VisibilityState)
        (origin: GridPos)
        (halfWidth: int)
        (halfHeight: int)
        (translucencyBudget: int)
        =
        if map.Width <> state.Width || map.Height <> state.Height then
            invalidArg "state" "VisibilityState size must match map dimensions."

        let mapWidth = map.Width
        let mapHeight = map.Height

        let halfW = if halfWidth < 0 then 0 else halfWidth
        let halfH = if halfHeight < 0 then 0 else halfHeight
        let budget =
            if translucencyBudget < 0 then 0
            elif translucencyBudget > 255 then 255
            else translucencyBudget

        state.BeginStep()

        let currentTick = state.CurrentTick
        let visibleStamp = state.VisibleStampArray
        let translucencyCost = state.TranslucencyCostArray

        if origin.X >= 0 && origin.X < mapWidth && origin.Y >= 0 && origin.Y < mapHeight then
            stampVisible map mapWidth origin.X origin.Y 0uy currentTick visibleStamp translucencyCost
        else
            ()

        if origin.X >= 0 && origin.X < mapWidth && origin.Y >= 0 && origin.Y < mapHeight then
            let ox = origin.X
            let oy = origin.Y

            let minX = max 0 (ox - halfW)
            let maxX = min (mapWidth - 1) (ox + halfW)
            let minY = max 0 (oy - halfH)
            let maxY = min (mapHeight - 1) (oy + halfH)

            if minX <= maxX && minY <= maxY then
                for octant in 0 .. 7 do
                    let struct (maxDepth, maxMinorForOctant, majorDx, majorDy, minorDx, minorDy) =
                        match octant with
                        | 0 -> struct (maxX - ox, oy - minY, 1, 0, 0, -1)
                        | 1 -> struct (oy - minY, maxX - ox, 0, -1, 1, 0)
                        | 2 -> struct (oy - minY, ox - minX, 0, -1, -1, 0)
                        | 3 -> struct (ox - minX, oy - minY, -1, 0, 0, -1)
                        | 4 -> struct (ox - minX, maxY - oy, -1, 0, 0, 1)
                        | 5 -> struct (maxY - oy, ox - minX, 0, 1, -1, 0)
                        | 6 -> struct (maxY - oy, maxX - ox, 0, 1, 1, 0)
                        | _ -> struct (maxX - ox, maxY - oy, 1, 0, 0, 1)

                    let mutable active = state.IntervalBufferA
                    let mutable scratch = state.IntervalBufferB
                    let mutable activeCount = 1

                    active.[0] <-
                        { Start = Slope.zero
                          End = Slope.one
                          Cost = 0uy }

                    let mutable depth = 1

                    while depth <= maxDepth && activeCount > 0 do
                        let mutable minor = 0
                        let maxMinor = min depth maxMinorForOctant
                        let mutable intervalHint = 0
                        let mutable x = ox + (depth * majorDx)
                        let mutable y = oy + (depth * majorDy)

                        while minor <= maxMinor && activeCount > 0 do
                            let struct (spanStart, spanEnd) = tileSpan depth minor
                            let struct (bestCost, nextHint) =
                                minOverlappingCostFrom active activeCount spanStart spanEnd intervalHint

                            intervalHint <- nextHint

                            if bestCost <> Int32.MaxValue then
                                let tileCost = byte bestCost
                                stampVisible map mapWidth x y tileCost currentTick visibleStamp translucencyCost

                                let idx = index mapWidth x y
                                let opacity = map.GetOpacityByIndex(idx)

                                match opacity with
                                | TileOpacity.Opaque ->
                                    let nextCount = applyOccluderSpan active activeCount scratch spanStart spanEnd opacity budget
                                    let tmp = active
                                    active <- scratch
                                    scratch <- tmp
                                    activeCount <- nextCount
                                    intervalHint <- 0
                                | TileOpacity.Translucent ->
                                    let nextCount = applyOccluderSpan active activeCount scratch spanStart spanEnd opacity budget
                                    let tmp = active
                                    active <- scratch
                                    scratch <- tmp
                                    activeCount <- nextCount
                                    intervalHint <- 0
                                | TileOpacity.Transparent
                                | TileOpacity.Air -> ()
                                | _ ->
                                    let nextCount = applyOccluderSpan active activeCount scratch spanStart spanEnd TileOpacity.Opaque budget
                                    let tmp = active
                                    active <- scratch
                                    scratch <- tmp
                                    activeCount <- nextCount
                                    intervalHint <- 0

                            x <- x + minorDx
                            y <- y + minorDy
                            minor <- minor + 1

                        depth <- depth + 1

    let inline isVisible (state: VisibilityState) (map: TileMap) (x: int) (y: int) =
        if x < 0 || x >= map.Width || y < 0 || y >= map.Height then false
        else state.IsVisible(index map.Width x y)

    let inline translucencyDepth (state: VisibilityState) (map: TileMap) (x: int) (y: int) =
        if x < 0 || x >= map.Width || y < 0 || y >= map.Height then -1
        else
            let idx = index map.Width x y
            if state.IsVisible(idx) then
                let cost = state.GetTranslucencyCost(idx)
                if map.IsExplored(x, y) then
                    match map.GetOpacity(x, y) with
                    | TileOpacity.Opaque
                    | TileOpacity.Translucent -> 0
                    | _ -> cost
                else
                    cost
            else
                -1

    let inline isExplored (state: VisibilityState) (map: TileMap) (x: int) (y: int) =
        let _ = state
        map.IsExplored(x, y)
