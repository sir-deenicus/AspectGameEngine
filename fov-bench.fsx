#r "./bin/Debug/net8.0/AspectGameEngine.dll"

open System
open System.Diagnostics
open AspectGameEngine

let registerTileset (name: string) =
    let props = TilePropertiesReference(name)

    let floorLoc = SpriteLoc(0, 0, 0)
    let wallLoc = SpriteLoc(0, 0, 1)
    let glassLoc = SpriteLoc(0, 0, 2)

    props.[floorLoc] <- { TileProperties.NullTile with Walkable = true; TileOpacity = TileOpacity.Transparent }
    props.[wallLoc] <- { TileProperties.NullTile with Walkable = false; TileOpacity = TileOpacity.Opaque }
    props.[glassLoc] <- { TileProperties.NullTile with Walkable = true; TileOpacity = TileOpacity.Translucent }

    TilesetRegistry.register name props

    floorLoc, wallLoc, glassLoc

let fillMap (map: TileMap) (floorLoc: SpriteLoc) (wallLoc: SpriteLoc) (glassLoc: SpriteLoc) =
    // Deterministic pattern: outer walls, some translucent stripes.
    for y in 0 .. map.Height - 1 do
        for x in 0 .. map.Width - 1 do
            let isBorder = x = 0 || y = 0 || x = map.Width - 1 || y = map.Height - 1
            let loc =
                if isBorder then wallLoc
                elif (x % 17 = 0) && (y % 3 = 0) then glassLoc
                else floorLoc

            map.Update(x, y, { SpriteLoc = loc; Health = 0; IsOccupied = false })

let bench (label: string) (map: TileMap) (state: VisibilityState) (origin: GridPos) (halfW: int) (halfH: int) (budget: int) (iters: int) =
    // Warmup
    for _ in 1 .. 50 do
        RectFov.compute map state origin halfW halfH budget

    let sw = Stopwatch.StartNew()
    for _ in 1 .. iters do
        RectFov.compute map state origin halfW halfH budget
    sw.Stop()

    let per = sw.Elapsed.TotalMilliseconds / float iters
    printfn "%s: %.4f ms/compute (iters=%d, half=%dx%d, budget=%d)" label per iters halfW halfH budget

let tilesetName = "bench"
let floorLoc, wallLoc, glassLoc = registerTileset tilesetName

let mapW, mapH = 300, 300
let map = TileMap(mapW, mapH, SpriteLoc(0,0,0), tilesetName, "bench", MapType.Room)
map.InitEffectiveOpacityCache()
fillMap map floorLoc wallLoc glassLoc

let state = VisibilityState(mapW, mapH)
let origin = GridPos(mapW / 2, mapH / 2)

let runAllBenches (map: TileMap) (state: VisibilityState) (origin: GridPos) =
    printfn "FOV type: interval visibility engine with surface disclosure"

    bench "1080p-ish" map state origin 20 10 2 2000
    bench "1440p-ish" map state origin 26 13 2 2000
    bench "4k-ish"    map state origin 40 20 2 1500
    bench "stress"    map state origin 60 40 2 500
    printfn ""

runAllBenches map state origin

(*
Results:
FOV type: Opus two phase approach (v0.2)
1080p-ish: 1.6243 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.1071 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 4.2854 ms/compute (iters=1500, half=40x20, budget=2)
stress: 11.8505 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2
1080p-ish: 1.5886 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.4368 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 4.1572 ms/compute (iters=1500, half=40x20, budget=2)
stress: 12.8098 ms/compute (iters=500, half=60x40, budget=2)

FOV type: gpt5.2 current (v0.1)
1080p-ish: 1.6773 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.4300 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 4.1750 ms/compute (iters=1500, half=40x20, budget=2)
stress: 10.2250 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.1
1080p-ish: 1.7141 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.3950 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 4.3236 ms/compute (iters=1500, half=40x20, budget=2)
stress: 11.8578 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2
1080p-ish: 1.9529 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.6149 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 5.5384 ms/compute (iters=1500, half=40x20, budget=2)
stress: 15.1793 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2
1080p-ish: 1.8233 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.5259 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 5.6553 ms/compute (iters=1500, half=40x20, budget=2)
stress: 14.5616 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.1
1080p-ish: 1.9879 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.5440 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 5.5301 ms/compute (iters=1500, half=40x20, budget=2)
stress: 14.7314 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.3 (gpt5.2 "supercover" apprcoach)
1080p-ish: 2.8625 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 4.0109 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 9.9832 ms/compute (iters=1500, half=40x20, budget=2)
stress: 29.6860 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.3 (gpt5.2 "supercover" approach)
1080p-ish: 3.1309 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 4.6130 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 11.0692 ms/compute (iters=1500, half=40x20, budget=2)
stress: 33.3149 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.3b (gpt5.2 "supercover" approach, relaxed)
1080p-ish: 2.0914 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.8932 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 6.8532 ms/compute (iters=1500, half=40x20, budget=2)
stress: 19.5034 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2
1080p-ish: 2.1102 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.9681 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 6.1365 ms/compute (iters=1500, half=40x20, budget=2)
stress: 15.1826 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2
1080p-ish: 1.8578 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.4843 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 5.5832 ms/compute (iters=1500, half=40x20, budget=2)
stress: 14.9824 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2b Opus approach with local fill
1080p-ish: 2.0441 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.7134 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 5.6604 ms/compute (iters=1500, half=40x20, budget=2)
stress: 16.2050 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2b Opus approach with local fill
1080p-ish: 2.2616 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.9557 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 6.0806 ms/compute (iters=1500, half=40x20, budget=2)
stress: 17.5140 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2c Opus approach with local fill and shallow slope pinhole correction
1080p-ish: 2.3416 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 3.0476 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 6.4624 ms/compute (iters=1500, half=40x20, budget=2)
stress: 17.6776 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2c Opus approach with local fill and shallow slope pinhole correction
1080p-ish: 2.0552 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 2.9557 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 6.0700 ms/compute (iters=1500, half=40x20, budget=2)
stress: 16.2638 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2d Opus approach with local fill and shallow slope pinhole correction, get opacity optimization
1080p-ish: 0.2712 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 0.4359 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 1.1669 ms/compute (iters=1500, half=40x20, budget=2)
stress: 3.1375 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2d Opus approach with local fill and shallow slope pinhole correction, get opacity optimization
1080p-ish: 0.2580 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 0.4147 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 0.9052 ms/compute (iters=1500, half=40x20, budget=2)
stress: 2.5921 ms/compute (iters=500, half=60x40, budget=2)

FOV type: v0.2d Opus approach with local fill and shallow slope pinhole correction, get opacity optimization
1080p-ish: 0.3182 ms/compute (iters=2000, half=20x10, budget=2)
1440p-ish: 0.5194 ms/compute (iters=2000, half=26x13, budget=2)
4k-ish: 0.8226 ms/compute (iters=1500, half=40x20, budget=2)
stress: 2.2526 ms/compute (iters=500, half=60x40, budget=2)
*)
