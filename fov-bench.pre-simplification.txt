namespace AspectGameEngine

open System
open System.Runtime.CompilerServices

type VisibilityState(width: int, height: int) =
    let visibleStamp = Array.zeroCreate<int> (width * height)
    let translucencyCost = Array.create<byte> (width * height) 255uy
    let postPassStamp = Array.zeroCreate<int> (width * height)
    let postPassCost = Array.create<byte> (width * height) 255uy
    let mutable tick = 1
    let mutable postPassTick = 1

    member _.Width = width
    member _.Height = height

    member _.BeginStep() =
        if tick = System.Int32.MaxValue then
            System.Array.Clear(visibleStamp, 0, visibleStamp.Length)
            tick <- 1
        else
            tick <- tick + 1 

    member _.IsVisible(index: int) =
        visibleStamp.[index] = tick

    member _.GetTranslucencyCost(index: int) =
        if visibleStamp.[index] = tick then int translucencyCost.[index]
        else -1

    member _.BeginPostPass() =
        if postPassTick = System.Int32.MaxValue then
            System.Array.Clear(postPassStamp, 0, postPassStamp.Length)
            postPassTick <- 1
        else
            postPassTick <- postPassTick + 1 

    member internal _.VisibleStampArray = visibleStamp
    member internal _.TranslucencyCostArray = translucencyCost
    member internal _.PostPassStampArray = postPassStamp
    member internal _.PostPassCostArray = postPassCost
    member internal _.CurrentTick = tick
    member internal _.CurrentPostPassTick = postPassTick

module RectFov =
    // Heuristic post-passes tended to create artifacts; instead we use an
    // adaptive refinement pass that only adds *real* LOS rays.
    // For very large visibility windows, the refinement pass scales poorly.
    // In those cases we fall back to a cheap crack-fill post-pass.
    let private enablePostPass = true

    // --- Strictness toggles ---
    // These two features intentionally make the rasterization more permissive
    // to reduce single-tile pinholes/jaggies, but they can also make doorways
    // look more rectangular (tiles adjacent to the opening become visible a
    // bit earlier than strict LOS would allow).
    //
    // Keep these off if we want stricter geometry; turn them on if we prefer
    // smoother silhouettes.
    let private enableDiagonalTouchMarking = false
    let private enableNonOpaquePinholeFill = false

    let private refinementMaxWindowTiles = 6000
    let private refinementMaxExtraRays = 2048

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private index (width: int) (x: int) (y: int) = y * width + x

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private blocksSight (opacity: TileOpacity) =
        match opacity with
        | TileOpacity.Opaque -> true
        | TileOpacity.Translucent -> false
        | TileOpacity.Transparent -> false
        | TileOpacity.Air -> false
        | _ -> true

    let private castRay
        (map: TileMap)
        (mapWidth: int)
        (minX: int)
        (maxX: int)
        (minY: int)
        (maxY: int)
        (budget: int)
        (originX: int)
        (originY: int)
        (targetX: int)
        (targetY: int)
        (currentTick: int)
        (visibleStamp: Span<int>)
        (translucencyCost: Span<byte>)
        =
        let mutable x = originX
        let mutable y = originY
        let mutable idx = index mapWidth originX originY

        let dx = abs (targetX - originX)
        let dy = abs (targetY - originY)
        let sx = if originX < targetX then 1 elif originX > targetX then -1 else 0
        let sy = if originY < targetY then 1 elif originY > targetY then -1 else 0
        let syw = sy * mapWidth
        let mutable err = dx - dy

        let mutable translucentCount = 0
        let mutable doneRay = false

        while not doneRay do
            if x < minX || x > maxX || y < minY || y > maxY then
                doneRay <- true
            else
                let opacity = map.GetOpacityByIndex(idx)

                // IMPORTANT SEMANTICS:
                // - The translucency depth is the number of translucent tiles we've already passed through.
                // - A translucent tile itself is visible at the *current* depth; the depth increases only
                //   after we pass through it (affecting tiles behind it).
                let tileCost = translucentCount

                let visibleAt = visibleStamp.[idx]
                let translucentByte = byte tileCost

                if visibleAt <> currentTick then
                    visibleStamp.[idx] <- currentTick
                    translucencyCost.[idx] <- translucentByte
                elif translucentByte < translucencyCost.[idx] then
                    translucencyCost.[idx] <- translucentByte

                if translucentByte = 0uy then
                    map.MarkExplored(x, y)

                match opacity with
                | TileOpacity.Opaque ->
                    doneRay <- true
                | TileOpacity.Translucent ->
                    if not (x = originX && y = originY) then
                        let nextCost = translucentCount + 1
                        if nextCost > budget then
                            doneRay <- true
                        else
                            translucentCount <- nextCost
                | _ -> ()

            if not doneRay then
                if x = targetX && y = targetY then
                    doneRay <- true
                else
                    let e2 = 2 * err
                    let mutable deltaIdx = 0

                    // When a discrete ray steps diagonally, the geometric line passes through a grid corner.
                    // Classic Bresenham can skip the two corner-touch cells, which shows up as shallow-slope
                    // pinholes (eg a transparent tile at y+1 missing when x-delta is large).
                    // We mark those touched cells visible at the current depth (no extra depth consumption).
                    let stepX = e2 > -dy
                    let stepY = e2 < dx
                    if enableDiagonalTouchMarking && stepX && stepY then
                        let touchByte = byte translucentCount
                        // Touch cell in X direction: (x+sx, y)
                        let tx = x + sx
                        if tx >= minX && tx <= maxX then
                            let tidx = idx + sx
                            let vAt = visibleStamp.[tidx]
                            if vAt <> currentTick then
                                visibleStamp.[tidx] <- currentTick
                                translucencyCost.[tidx] <- touchByte
                            elif touchByte < translucencyCost.[tidx] then
                                translucencyCost.[tidx] <- touchByte

                            if touchByte = 0uy then
                                map.MarkExplored(tx, y)

                        // Touch cell in Y direction: (x, y+sy)
                        let ty = y + sy
                        if ty >= minY && ty <= maxY then
                            let tidx = idx + syw
                            let vAt = visibleStamp.[tidx]
                            if vAt <> currentTick then
                                visibleStamp.[tidx] <- currentTick
                                translucencyCost.[tidx] <- touchByte
                            elif touchByte < translucencyCost.[tidx] then
                                translucencyCost.[tidx] <- touchByte

                            if touchByte = 0uy then
                                map.MarkExplored(x, ty)

                    if stepX then
                        err <- err - dy
                        x <- x + sx
                        deltaIdx <- deltaIdx + sx

                    if stepY then
                        err <- err + dx
                        y <- y + sy
                        deltaIdx <- deltaIdx + syw

                    idx <- idx + deltaIdx

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let inline private tryFillNonOpaquePinhole
        (map: TileMap)
        (mapWidth: int)
        (minX: int)
        (maxX: int)
        (minY: int)
        (maxY: int)
        (x: int)
        (y: int)
        (idx: int)
        (currentTick: int)
        (visibleStamp: Span<int>)
        (translucencyCost: Span<byte>)
        : struct (bool * byte) =

        // We only fill when we're sure this is a 1-tile pinhole, not a corner-peek.
        // Return: (shouldFill, candidateCost).

        let mutable filled = false
        let mutable candidate = 255uy

        // Opposite-sides bracket: [V][?][V] horizontally or vertically.
        if x > minX && x < maxX then
            let lidx = idx - 1
            let ridx = idx + 1
            if visibleStamp.[lidx] = currentTick && visibleStamp.[ridx] = currentTick then
                let lo = map.GetOpacityByIndex(lidx)
                let ro = map.GetOpacityByIndex(ridx)
                if lo <> TileOpacity.Opaque && ro <> TileOpacity.Opaque then
                    candidate <- min translucencyCost.[lidx] translucencyCost.[ridx]
                    filled <- true

        if not filled && y > minY && y < maxY then
            let uidx = idx - mapWidth
            let didx = idx + mapWidth
            if visibleStamp.[uidx] = currentTick && visibleStamp.[didx] = currentTick then
                let uo = map.GetOpacityByIndex(uidx)
                let do2 = map.GetOpacityByIndex(didx)
                if uo <> TileOpacity.Opaque && do2 <> TileOpacity.Opaque then
                    candidate <- min translucencyCost.[uidx] translucencyCost.[didx]
                    filled <- true

        // Corner pinhole: two adjacent visible non-opaque neighbors, other two sides opaque.
        // We also require the diagonal between the two visible neighbors to be visible and non-opaque,
        // which prevents "seeing behind walls" through a doorway around a solid corner.
        if not filled && x > minX && x < maxX && y > minY && y < maxY then
            let lidx = idx - 1
            let ridx = idx + 1
            let uidx = idx - mapWidth
            let didx = idx + mapWidth

            let inline nonOpaqueIdx (i: int) = map.GetOpacityByIndex(i) <> TileOpacity.Opaque
            let inline opaqueIdx (i: int) = map.GetOpacityByIndex(i) = TileOpacity.Opaque

            // Left + Up corner
            if not filled
               && visibleStamp.[lidx] = currentTick && visibleStamp.[uidx] = currentTick
               && nonOpaqueIdx lidx && nonOpaqueIdx uidx
               && opaqueIdx ridx && opaqueIdx didx then
                let didx2 = idx - mapWidth - 1
                if visibleStamp.[didx2] = currentTick && nonOpaqueIdx didx2 then
                    candidate <- min (min translucencyCost.[lidx] translucencyCost.[uidx]) translucencyCost.[didx2]
                    filled <- true

            // Up + Right corner
            if not filled
               && visibleStamp.[uidx] = currentTick && visibleStamp.[ridx] = currentTick
               && nonOpaqueIdx uidx && nonOpaqueIdx ridx
               && opaqueIdx lidx && opaqueIdx didx then
                let didx2 = idx - mapWidth + 1
                if visibleStamp.[didx2] = currentTick && nonOpaqueIdx didx2 then
                    candidate <- min (min translucencyCost.[uidx] translucencyCost.[ridx]) translucencyCost.[didx2]
                    filled <- true

            // Right + Down corner
            if not filled
               && visibleStamp.[ridx] = currentTick && visibleStamp.[didx] = currentTick
               && nonOpaqueIdx ridx && nonOpaqueIdx didx
               && opaqueIdx lidx && opaqueIdx uidx then
                let didx2 = idx + mapWidth + 1
                if visibleStamp.[didx2] = currentTick && nonOpaqueIdx didx2 then
                    candidate <- min (min translucencyCost.[ridx] translucencyCost.[didx]) translucencyCost.[didx2]
                    filled <- true

            // Down + Left corner
            if not filled
               && visibleStamp.[didx] = currentTick && visibleStamp.[lidx] = currentTick
               && nonOpaqueIdx didx && nonOpaqueIdx lidx
               && opaqueIdx ridx && opaqueIdx uidx then
                let didx2 = idx + mapWidth - 1
                if visibleStamp.[didx2] = currentTick && nonOpaqueIdx didx2 then
                    candidate <- min (min translucencyCost.[didx] translucencyCost.[lidx]) translucencyCost.[didx2]
                    filled <- true

        struct (filled, candidate)

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
        let visibleStamp = Span<int>(state.VisibleStampArray)
        let translucencyCost = Span<byte>(state.TranslucencyCostArray)

        if origin.X >= 0 && origin.X < mapWidth && origin.Y >= 0 && origin.Y < mapHeight then
            let originIndex = index mapWidth origin.X origin.Y
            if visibleStamp.[originIndex] <> currentTick then
                visibleStamp.[originIndex] <- currentTick
                translucencyCost.[originIndex] <- 0uy
            elif 0uy < translucencyCost.[originIndex] then
                translucencyCost.[originIndex] <- 0uy
            map.MarkExplored(origin.X, origin.Y)

        let ox = origin.X
        let oy = origin.Y

        let minX = max 0 (ox - halfW)
        let maxX = min (mapWidth - 1) (ox + halfW)
        let minY = max 0 (oy - halfH)
        let maxY = min (mapHeight - 1) (oy + halfH)

        let windowW = (maxX - minX) + 1
        let windowH = (maxY - minY) + 1
        let windowTiles = windowW * windowH

        let useRefinement = windowTiles <= refinementMaxWindowTiles

        for x in minX .. maxX do
            castRay map mapWidth minX maxX minY maxY budget ox oy x minY currentTick visibleStamp translucencyCost
            castRay map mapWidth minX maxX minY maxY budget ox oy x maxY currentTick visibleStamp translucencyCost

        if maxY - minY > 1 then
            for y in minY + 1 .. maxY - 1 do
                castRay map mapWidth minX maxX minY maxY budget ox oy minX y currentTick visibleStamp translucencyCost
                castRay map mapWidth minX maxX minY maxY budget ox oy maxX y currentTick visibleStamp translucencyCost

        // Adaptive refinement pass (quality mode):
        // Perimeter-only rays can miss a few tiles (pinholes). When the visibility
        // window is reasonably small (viewport-sized), we cast extra *real* LOS rays
        // only to non-opaque tiles that touch an already-visible tile.
        // Opaque-border solidify runs AFTER, as a separate pass, so that all
        // non-opaque supporting tiles are resolved first (avoids scan-order bugs).
        if useRefinement then
            let mutable raysLeft = refinementMaxExtraRays

            for y in minY .. maxY do
                if raysLeft > 0 then
                    for x in minX .. maxX do
                        if raysLeft > 0 then
                            let idx = index mapWidth x y
                            if visibleStamp.[idx] <> currentTick then
                                if map.GetOpacityByIndex(idx) <> TileOpacity.Opaque then
                                    let mutable needsRay = false

                                    if x > minX && visibleStamp.[idx - 1] = currentTick then needsRay <- true
                                    elif x < maxX && visibleStamp.[idx + 1] = currentTick then needsRay <- true
                                    elif y > minY && visibleStamp.[idx - mapWidth] = currentTick then needsRay <- true
                                    elif y < maxY && visibleStamp.[idx + mapWidth] = currentTick then needsRay <- true

                                    if needsRay then
                                        castRay map mapWidth minX maxX minY maxY budget ox oy x y currentTick visibleStamp translucencyCost
                                        raysLeft <- raysLeft - 1


        // Crack-fill post-pass (speed mode):
        // Only run this when refinement is disabled for large windows.
        if enablePostPass && not useRefinement then
            state.BeginPostPass()

            let currentPostPassTick = state.CurrentPostPassTick
            let postPassStamp = Span<int>(state.PostPassStampArray)
            let postPassCost = Span<byte>(state.PostPassCostArray)

            for y in minY .. maxY do
                for x in minX .. maxX do
                    let idx = index mapWidth x y

                    if visibleStamp.[idx] <> currentTick then
                        let opacity = map.GetOpacityByIndex(idx)

                        if opacity = TileOpacity.Opaque then
                            let mutable candidateCost = System.Int32.MaxValue

                            if x > minX && x < maxX then
                                let lidx = idx - 1
                                let ridx = idx + 1
                                if visibleStamp.[lidx] = currentTick && visibleStamp.[ridx] = currentTick then
                                    if translucencyCost.[lidx] = 0uy && translucencyCost.[ridx] = 0uy then
                                        candidateCost <- 0

                            if y > minY && y < maxY then
                                let uidx = idx - mapWidth
                                let didx = idx + mapWidth
                                if visibleStamp.[uidx] = currentTick && visibleStamp.[didx] = currentTick then
                                    if translucencyCost.[uidx] = 0uy && translucencyCost.[didx] = 0uy then
                                        if 0 < candidateCost then candidateCost <- 0

                            if candidateCost <> System.Int32.MaxValue && candidateCost <= budget then
                                let candidateByte = byte candidateCost
                                if postPassStamp.[idx] <> currentPostPassTick then
                                    postPassStamp.[idx] <- currentPostPassTick
                                    postPassCost.[idx] <- candidateByte
                                elif candidateByte < postPassCost.[idx] then
                                    postPassCost.[idx] <- candidateByte

            for y in minY .. maxY do
                for x in minX .. maxX do
                    let idx = index mapWidth x y

                    if postPassStamp.[idx] = currentPostPassTick then
                        let candidateByte = postPassCost.[idx]
                        if visibleStamp.[idx] <> currentTick then
                            visibleStamp.[idx] <- currentTick
                            translucencyCost.[idx] <- candidateByte
                        elif candidateByte < translucencyCost.[idx] then
                            translucencyCost.[idx] <- candidateByte

                        if candidateByte = 0uy then 
                            map.MarkExplored(x, y)

        // Final pass: conservative gap-fill.
        // Runs AFTER all non-opaque visibility is settled (refinement rays or
        // crack-fill). This fills occasional pinholes where a wall/glass tile is
        // adjacent (including diagonals) to visible clear space, but was missed
        // by perimeter rays due to grid discretization.
        // Cost: stamp reads + GetOpacity calls only, no raycasting.
        for y in minY .. maxY do
            for x in minX .. maxX do
                let idx = index mapWidth x y
                if visibleStamp.[idx] <> currentTick then
                    let opacity = map.GetOpacityByIndex(idx)
                    if opacity = TileOpacity.Opaque || opacity = TileOpacity.Translucent then
                        let mutable found = false

                        if x > minX then
                            let nidx = idx - 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && x < maxX then
                            let nidx = idx + 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && y > minY then
                            let nidx = idx - mapWidth
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && y < maxY then
                            let nidx = idx + mapWidth
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true

                        // Diagonal support for corner cases (common with door/window corners).
                        if not found && x > minX && y > minY then
                            let nidx = idx - mapWidth - 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && x < maxX && y > minY then
                            let nidx = idx - mapWidth + 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && x > minX && y < maxY then
                            let nidx = idx + mapWidth - 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true
                        if not found && x < maxX && y < maxY then
                            let nidx = idx + mapWidth + 1
                            if visibleStamp.[nidx] = currentTick && translucencyCost.[nidx] = 0uy then
                                let nOpacity = map.GetOpacityByIndex(nidx)
                                if nOpacity <> TileOpacity.Opaque then
                                    found <- true

                        if found then
                            visibleStamp.[idx] <- currentTick
                            translucencyCost.[idx] <- 0uy
                            map.MarkExplored(x, y)
                    else
                        // Priority: optional non-opaque pinhole fill.
                        // If enabled and it fills, we skip all other transparent heuristics.
                        let mutable filledOptional = false
                        if enableNonOpaquePinholeFill then
                            let struct (filled, candidate) =
                                tryFillNonOpaquePinhole map mapWidth minX maxX minY maxY x y idx currentTick visibleStamp translucencyCost

                            if filled then
                                visibleStamp.[idx] <- currentTick
                                translucencyCost.[idx] <- candidate
                                if candidate = 0uy then
                                    map.MarkExplored(x, y)
                                filledOptional <- true

                        // Always-on (fallback): bracketed transparent tiles.
                        // If a transparent/air tile is bracketed by visible Opaque/Translucent tiles
                        // (eg a doorway between walls), and BOTH brackets are fully visible (cost=0),
                        // make this tile fully visible too.
                        if not filledOptional then
                            if opacity = TileOpacity.Transparent || opacity = TileOpacity.Air then
                                let mutable filledTransparentFull = false

                                if x > minX && x < maxX then
                                    let lidx = idx - 1
                                    let ridx = idx + 1
                                    if visibleStamp.[lidx] = currentTick && visibleStamp.[ridx] = currentTick then
                                        if translucencyCost.[lidx] = 0uy && translucencyCost.[ridx] = 0uy then
                                            let lo = map.GetOpacityByIndex(lidx)
                                            let ro = map.GetOpacityByIndex(ridx)
                                            if (lo = TileOpacity.Opaque || lo = TileOpacity.Translucent)
                                               && (ro = TileOpacity.Opaque || ro = TileOpacity.Translucent) then
                                                visibleStamp.[idx] <- currentTick
                                                translucencyCost.[idx] <- 0uy
                                                map.MarkExplored(x, y)
                                                filledTransparentFull <- true

                                if not filledTransparentFull && y > minY && y < maxY then
                                    let uidx = idx - mapWidth
                                    let didx = idx + mapWidth
                                    if visibleStamp.[uidx] = currentTick && visibleStamp.[didx] = currentTick then
                                        if translucencyCost.[uidx] = 0uy && translucencyCost.[didx] = 0uy then
                                            let uo = map.GetOpacityByIndex(uidx)
                                            let do2 = map.GetOpacityByIndex(didx)
                                            if (uo = TileOpacity.Opaque || uo = TileOpacity.Translucent)
                                               && (do2 = TileOpacity.Opaque || do2 = TileOpacity.Translucent) then
                                                visibleStamp.[idx] <- currentTick
                                                translucencyCost.[idx] <- 0uy
                                                map.MarkExplored(x, y)
                                                filledTransparentFull <- true

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
