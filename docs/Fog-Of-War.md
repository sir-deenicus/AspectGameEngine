# Fog Of War And Visibility

This document explains how the current fog-of-war and field-of-view system works. The implementation lives in `FOV.fs`.

The short version: this is not ordinary binary fog of war. Each compute step finds currently visible tiles inside a rectangular view, remembers a translucency depth for those tiles, and marks fully visible tiles as permanently explored on the `TileMap`. Translucent tiles such as glass are visible themselves, but they spend a limited budget for the tiles behind them.

## Explanation

The live system is built around `RectFov.compute`. The caller gives it a runtime `TileMap`, a reusable `VisibilityState`, an origin position, a rectangular half-width and half-height, and a translucency budget.

The result has two layers of meaning.

Current visibility is temporary. A tile is currently visible when its entry in `VisibilityState` was stamped during the latest compute step.

Exploration is persistent. A tile is explored when `TileMap.Explored` has been marked. Exploration survives later visibility recomputes until the map's explored grid is explicitly cleared.

Translucency is the third piece. Visible cells carry a cost that means "how many translucent cells did the ray already pass through before reaching this cell?" A cost of `0` is fully clear visibility. A cost greater than `0` means the tile is visible only through one or more translucent cells.

## Data It Owns

`VisibilityState` owns reusable arrays sized to the map:

- `visibleStamp`: the current-visibility stamp for each cell.
- `translucencyCost`: the current step's lowest known translucency cost for each visible cell.
- `intervalBufferA`: reusable interval storage for the active octant.
- `intervalBufferB`: reusable scratch interval storage while applying an occluder.

The state uses incrementing integer ticks instead of clearing the visibility arrays every frame. Calling `BeginStep()` advances the visibility tick; cells stamped with that tick are visible for the current step. If the tick ever reaches `Int32.MaxValue`, the stamp array is cleared and the tick restarts.

The map owns exploration. `TileMap.MarkExplored(x, y)` writes to `TileMap.Explored`, and `RectFov.isExplored` simply reads that map-owned state. This is important: visibility state can be reused, but explored state belongs to the runtime map.

The system uses the same flat indexing convention as the rest of the map code:

```text
index = y * width + x
```

## Tile Opacity

The FOV code reads opacity through `TileMap.GetOpacityByIndex`, which uses the map's effective opacity cache. Base tile opacity and relevant layer entities should already have been folded into that cache by the map layer.

Current opacity behavior:

- `Opaque` is visible when reached, then removes its angular span from intervals behind it.
- `Transparent` does not block and does not spend translucency budget.
- `Air` does not block and does not spend translucency budget.
- `Translucent` is visible at the interval's current cost, then propagates the overlapped span behind it at cost plus one while budget remains.

The important translucent rule is subtle: a translucent tile itself is visible at the current depth. The depth increases only after visibility passes behind that tile. With a clear interval entering a glass tile, the glass tile has cost `0`; the tile behind it has cost `1`.

This is the core behavior the current system is preserving. Translucent material lets the player see behind it for a short distance, and that sight is not as clear as direct sight, but the translucent tile itself should still be cleanly visible. The difficult part is keeping neighboring tiles from popping in too early or disappearing at shallow angles.

## Compute Flow

`RectFov.compute` first validates that the `VisibilityState` dimensions match the map. It clamps negative half-width, half-height, and translucency budget values into usable ranges, then stamps the origin as visible at cost `0` when the origin is inside the map.

The visible window is rectangular and clipped to map bounds:

```text
minX = max 0 (originX - halfWidth)
maxX = min (mapWidth - 1) (originX + halfWidth)
minY = max 0 (originY - halfHeight)
maxY = min (mapHeight - 1) (originY + halfHeight)
```

The main pass processes eight octants around the origin. Each octant starts with one open angular interval from slope `0` to slope `1` at cost `0`. The implementation walks outward by octant-local depth, then across the minor axis inside that column. Axis and diagonal seam tiles can be visited by two octants; stamping is idempotent and keeps the lowest cost.

Every non-origin tile is treated as a full unit square, not as a center point. In the octant's slope space, the tile subtends an angular span. A tile is visible when that span has positive-measure overlap with a surviving interval. Touching at only one endpoint is not visibility, which prevents sight leaking through two opaque tiles that meet only at a corner.

When a tile is visible, it is stamped with the lowest overlapping interval cost. Then opacity updates the interval list for later tiles in that octant. Opaque tiles subtract their full span. Translucent tiles split the overlapping span and propagate that middle span with cost plus one, unless the next cost would exceed `translucencyBudget`. Transparent and air tiles leave intervals unchanged.

There are no perimeter rays, adaptive refinement rays, large-window crack fill, wall-face fill, transparent bracket fill, structural-tile checks, or disabled smoothing toggles in the live compute path. The interval output is final.

## Translucency Budget

The `translucencyBudget` controls how many translucent cells a ray may pass through. It is clamped to the byte-sized range `0..255`.

This budget is not a wall opacity value and not an alpha value. It is a discrete pass-through count.

Examples:

- Budget `0`: glass itself can be seen from the clear side, but the ray stops before revealing tiles behind it.
- Budget `1`: the ray can reveal tiles behind one translucent tile.
- Budget `2`: the ray can reveal tiles behind two translucent tiles, and so on.

When more than one octant interval reaches the same tile, the lower translucency cost wins. A tile seen clearly at cost `0` remains cost `0` even if another interval later reaches it through glass.

## Exploration

Exploration is intentionally stricter than current visibility. During interval processing, cells are marked explored only when their effective visibility cost is `0`.

That means clear line-of-sight cells become permanently explored. Opaque and translucent faces can also become explored when seen directly. Cells visible only through translucent material may be currently visible, but they are not automatically explored unless some cost-`0` path reaches them.

`RectFov.translucencyDepth` has one extra presentation rule. If a currently visible tile is already explored and is itself `Opaque` or `Translucent`, the function reports depth `0`. Otherwise it reports the current visibility cost. Out-of-bounds or not-currently-visible cells return `-1`.

## How It Fits The Game

`GameUpdate.recomputeVisibility` is the current main caller. It passes the model's map, visibility state, player position, rectangular view size, and translucency budget into `RectFov.compute`.

The FOV code does not own map opacity. It expects `TileMap.EffectiveOpacity` to already represent the current base tile plus relevant layer state. If a door, fixture, spell, editor action, or gameplay system changes opacity, the map should update its effective opacity before visibility is recomputed.

The FOV code also does not serialize visibility state. Current visibility is frame/runtime state. Exploration is serialized with the runtime map through the map's explored grid.

## Performance Notes

The current approach is viewport-oriented. It processes octant intervals only inside the rectangular visible window instead of evaluating the whole map.

The current benchmark script is `fov-bench.fsx`. It uses a deterministic `300x300` map with opaque borders and translucent stripes, then measures several rectangular windows with translucency budget `2`.

Pre-rewrite snapshot from 2026-07-03 on this workspace, after restoring the old live implementation:

```text
1080p-ish: 0.2785 ms/compute (half=20x10)
1440p-ish: 0.6332 ms/compute (half=26x13)
4k-ish:    0.9653 ms/compute (half=40x20)
stress:    1.9999 ms/compute (half=60x40)
```

Older benchmark notes in `fov-bench.fsx` show that earlier v0.1, v0.2, v0.2b, v0.2c, v0.2d, and supercover experiments were either slower, more artifact-prone, or dependent on repair passes. Treat those inline results as history, not as labels for the live implementation.

Interval-engine verification range from two final 2026-07-03 runs after the rewrite and hot-path cleanup:

```text
1080p-ish: 0.2190-0.3243 ms/compute (half=20x10)
1440p-ish: 0.3724-0.5909 ms/compute (half=26x13)
4k-ish:    1.1943-1.7373 ms/compute (half=40x20)
stress:    3.3101-3.9818 ms/compute (half=60x40)
```

Those numbers are within the tier-0 gate of `2x` the pre-rewrite snapshot. The benchmark script now reports the interval engine directly and no longer prints the old fake supercover mode.

## Tradeoffs

The algorithm favors a rectangular viewport contract. This fits the current game update path and keeps work proportional to the area around the player, but it is not a general-purpose circular shadowcasting API.

The translucent budget is a gameplay/display concept, not physically correct light transport. It gives the renderer or game layer enough information to distinguish clear visibility from visibility through glass-like material, while keeping the compute path cheap.

The interval engine is more explicit than the old perimeter-ray implementation. It removes pinholes and fill artifacts by making the tile square's angular span the source of truth, but it does more interval bookkeeping than a simple ray perimeter. The hot path keeps reusable interval arrays on `VisibilityState`, clips each octant to the rectangular viewport, and keeps slope comparisons exact with integer cross-multiplication.

The new output intentionally differs from the old heuristic fills in some places. Oblique wall and glass faces can become visible when their square span has genuine overlap with the open interval. Doorway tiles are visible because their spans overlap, not because they are bracketed by walls. Tiles that were visible only because of old fill repairs may disappear if no positive-measure interval reaches them.

The implementation reads only effective opacity. It does not distinguish structural base tiles from opaque fixtures in `RectFov.compute`; that distinction existed only to keep the old face-fill pass from revealing fixtures too early.

## Interval Rewrite Plan

This was the plan used for the 2026-07-03 interval rewrite. It is retained here as the migration record and as context for future changes.

The chosen direction was interval-based visibility. Instead of casting rays to the edge of the rectangular view and then repairing missed cells, process cells outward from the player while tracking which angular intervals are still open. Every tile occupies an angular span as seen from the origin. If any open interval overlaps that span, the tile is visible. Opaque tiles are visible, then close the overlapped span behind them. Translucent tiles are visible, then continue the overlapped span with one more translucency cost. Transparent and air tiles pass the interval onward unchanged.

This changes the core question from "did one perimeter ray happen to hit this cell cleanly?" to "does any surviving visible slice of space overlap this cell?" That is closer to the gameplay concept and should reduce the need for pinhole fill, diagonal touch marking, and doorway-specific repairs.

The contract to preserve is:

- Current visibility remains temporary and stored in `VisibilityState`.
- Exploration remains map-owned and is marked only for cost `0` visibility.
- The visible area remains the rectangular viewport passed to `RectFov.compute`.
- A translucent tile is visible at the current cost before it spends budget.
- Cells behind translucent material are visible only while their interval cost is within `translucencyBudget`.
- If several paths reach a tile, the lowest translucency cost wins.
- Opaque fixtures on transparent floor are blockers, but they are not structural wall faces. They should be revealed only by real visibility, not by a wall-face cleanup rule.
- Public query behavior from `RectFov.isVisible`, `RectFov.isExplored`, and `RectFov.translucencyDepth` should stay stable unless a test intentionally changes it.

The important implementation detail is interval state. An open interval needs at least a start angle, an end angle, and a translucency cost. The algorithm can process rings around the origin, clipped to the current rectangular window. For each candidate tile, compute the tile's angular span using its near/far corners under a fixed corner policy. Intersect that span with the open intervals that reach the tile's distance band. The lowest surviving cost stamps the tile.

Opacity then updates interval state:

- `Transparent` and `Air`: keep the overlapping intervals unchanged.
- `Translucent`: keep the overlapping intervals, but with cost plus one for cells behind the tile; if that next cost would exceed budget, do not propagate those intervals farther.
- `Opaque`: do not propagate the overlapping intervals behind the tile.
- Unknown or future opacity values should default toward blocking, matching the current conservative `blocksSight` behavior.

The corner policy needs to be explicit and tested. A grid cell can be treated as a square area, not a center point. Doorway and window behavior should come from the square's angular coverage rather than from post-pass guesses. If the policy needs a small epsilon to avoid floating point edge flicker, that epsilon should live in one geometry helper with tests, not be scattered through visibility rules.

The interval implementation does not have to use floating point angles if profiling says not to. A slope-pair representation can describe angular spans with integer or rational comparisons, especially if the algorithm works per octant. The first implementation can favor clarity with benchmark coverage. If it is too slow, optimize the representation after the contract is already pinned down.

## Staged Plan

Stage 1 is to freeze the current contract harder. Keep the existing `fov-tests.fsx` cases and add cases for bracketed doorways, glass beside walls at shallow angles, direct visibility to an opaque fixture in open space, a fixture hidden behind a wall near a door, multiple translucent panes, and a large-window case that crosses the current `6000` tile strategy boundary. Record `fov-bench.fsx` timings before any rewrite work. These tests should describe what the game needs, not the current implementation's private cleanup passes.

Stage 2 is to separate geometry from map mutation. Add small internal helpers for rectangular window clipping, flat indexing, tile span calculation, interval intersection, and interval subtraction. Test these helpers without `TileMap` where possible. The goal is to make the hard part of the algorithm inspectable before it touches explored flags or effective opacity.

Stage 3 is to prototype the interval visibility engine beside the live engine, but not as a permanent private toggle. The comparison path can live in a temporary script or archived branch of code while the rewrite is under test. Correctness is judged against the characterization suite and the independent oracle mandated in the tier-0 work order below — **not** against stamp-for-stamp equality with the old engine, whose output includes the heuristic-fill artifacts this rewrite exists to remove.

Stage 4 is to wire the new engine into `RectFov.compute` while keeping the public API stable. `VisibilityState` should remain reusable and tick-based. `TileMap.MarkExplored` should still be called only when the chosen visibility cost is `0`. The first replacement should preserve the rectangular viewport contract rather than expanding into a circular or whole-map FOV API.

Stage 5 is to delete inactive and superseded pathways from the live hot path. Remove permanent false flags, optional smoothing branches, non-opaque pinhole fill, adaptive perimeter-ray refinement, and large-window crack fill once the interval engine covers their intended behavior. If an old approach remains useful as historical context, move it to `archive/` or describe it here; do not leave it as dormant executable code.

Stage 6 is to benchmark and tune. Compare the new implementation against the current viewport sizes in `fov-bench.fsx`: 1080p-ish, 1440p-ish, 4k-ish, and stress. If the interval version is slower, first look for representation and allocation problems. Keep the algorithmic contract intact while optimizing; do not reintroduce cleanup passes as performance shortcuts unless a test proves the rule is still coherent.

Stage 7 is to simplify the documentation after replacement. The current compute-flow section should become historical once the old perimeter-ray implementation is gone. The live explanation should describe the interval algorithm directly, and the history should record which artifacts disappeared, which tradeoffs remain, and which tests protect the translucency behavior.

## Tier-0 Work Order: Interval Visibility Engine (2026-07-03)

Tier-0 review verdict on the rewrite plan above: **direction approved**. Interval-based visibility
(beam casting with cost-carrying angular intervals) is the correct replacement — it makes pinholes
impossible by construction, so all four repair passes (adaptive refinement, crack fill, structural
face fill, transparent bracket fill) become unnecessary rather than merely relocated.

This section is the execution contract. A tier-1 executor implements from this document and the
repo alone. Decisions marked **locked** are implement-as-written; do not relitigate them in the
implementation. If a locked decision appears to be wrong during execution, stop and report — do not
quietly adapt.

### Locked decisions

**L1 — Corner policy (the load-bearing decision).**
The origin is the *center point* of the origin tile. Every other tile is a full unit square.
A tile is visible at cost `c` iff its angular span, as seen from the origin point, has
positive-measure overlap with a surviving open interval of cost `c` (touching at a single endpoint
is not visibility). A tile's angular span is the span subtended by the full square.

**L2 — Occlusion policy.**
An opaque tile, once reached, subtracts its *full* angular span from the open intervals behind it.
A translucent tile stamps itself at the interval's current cost, then propagates the overlapped
portion behind it at cost `+1`, or not at all if `cost + 1 > translucencyBudget`. Transparent and
Air propagate unchanged. Unknown opacity values block (match current `blocksSight`). Intervals of
zero width are discarded immediately: two opaque tiles meeting only at a corner leak no sight.

**L3 — Origin rules.**
The origin tile is stamped visible at cost `0` and marked explored unconditionally (current
behavior). The origin tile never spends translucency budget even if it is itself translucent
(current behavior — see the `not (x = originX && y = originY)` guard in the old `castRay`).

**L4 — Exact arithmetic, from the first prototype.**
No floating point in the core. Process per octant; represent interval endpoints as slope pairs
(rational numbers). Double all corner coordinates so tile corners become integers, and compare
slopes by `int64` cross-multiplication. This is not an optimization — it removes the entire
frame-to-frame flicker failure class that an epsilon would paper over, and it must be in place
*before* tests pin behavior, because retrofitting exactness after tests encode epsilon artifacts
is expensive. There is no epsilon anywhere; L1/L2's open/closed interval rules make it unnecessary.

**L5 — Processing order.**
Within each octant, process column by column outward from the origin (octant-local depth 1, 2, …),
clipped to the rectangular window. This guarantees every occluder is processed before any tile it
occludes. Tiles on octant seams (axes and diagonals) are visited by two octants; stamping is
idempotent min-cost so this is safe, but interval state is strictly per-octant and never shared.

**L6 — Interval state invariants.**
Per-octant interval lists are sorted and pairwise disjoint. Along any fixed angle, cost is
non-decreasing with distance (a consequence of L2 — assert it in debug builds if cheap). When a
tile's span is overlapped by intervals of different costs, the minimum cost stamps the tile.
A translucent tile splits an overlapping interval into at most three pieces (unchanged left,
cost+1 middle, unchanged right); merge adjacent same-cost intervals opportunistically. All interval
storage is preallocated and reused across computes — zero steady-state allocation, matching the
existing `VisibilityState` reuse discipline.

**L7 — Public contract unchanged.**
`RectFov.compute` signature, `VisibilityState` tick mechanics, `isVisible`, `isExplored`, and
`translucencyDepth` (including its explored-opaque/translucent-reports-depth-0 presentation rule)
stay exactly as they are. `MarkExplored` is called only when a tile is stamped at cost `0`.
The rectangular window clip is unchanged.

**L8 — No compensating passes, no structural special-casing.**
The interval engine's output is final. Do not add any post-pass that can mark a tile visible
outside the L1/L2 rules, and do not consult `GetTileProperties` vs `GetOpacityByIndex` to
distinguish structural faces from fixtures anywhere in compute — the new engine reads effective
opacity only. The structural/fixture distinction existed solely to keep the old face-fill pass
honest; with no face-fill pass it must have no representation in the code.

### Declared behavior changes (intended, not regressions)

The old engine's heuristic fills produced output that pure geometry will not reproduce. The
following differences are expected and correct; do not "fix" them:

- More oblique wall and glass faces become visible. A long corridor wall seen at a shallow angle
  is visible along its length because each face's span genuinely overlaps the open corridor
  interval. The old engine often missed these and patched some back with face fill.
- Doorway tiles are visible because their spans overlap, not because they are bracketed by walls.
  If some tile the old bracket fill revealed is geometrically invisible, it stays invisible.
- Single-tile pinholes cannot occur, so output near translucent cells at shallow angles will be
  smoother than the old perimeter-ray output.

### Enumerated traps

- **T1 — Porting the fills to match old output.** The strongest failure mode of this task: diffs
  against the old engine will not be empty, and the tempting fix is to re-add bracket/face fill
  logic. That silently rebuilds the architecture being deleted. Old-engine diffs are triage input
  only (see oracle rules); the fills must not survive in any form.
- **T2 — Same-ring occlusion bugs.** Processing whole Chebyshev rings without per-octant ordering
  lets a tile be tested before its occluder on the same ring near the diagonal. L5 exists to
  prevent this; do not substitute a plain ring loop.
- **T3 — Budget spend on the tile itself.** The translucent tile stamps at the *pre-spend* cost.
  Spending on arrival (instead of on pass-through) breaks `testTranslucentTileVisibleAtCurrentDepthWithZeroBudget`
  and the budget-exhaustion test. The old code got this right; keep its semantics exactly.
- **T4 — Corner-touch leaks.** If interval subtraction leaves zero-width or negative intervals,
  sight leaks through diagonal wall corners. L2's discard rule is mandatory; add a unit test with
  two opaque tiles sharing only a corner.
- **T5 — Seam double-spend.** A translucent tile on an octant seam is processed by two octants.
  Each octant's intervals are independent so no double budget spend can occur *unless* interval
  state is accidentally shared; L5/L6 forbid sharing.
- **T6 — The bookshelf regression.** `testOpaqueFixtureBehindWallNextToDoorStaysHidden` must pass
  from pure geometry (verified: no straight sightline from the origin reaches the bookshelf tile
  through the doorway in that fixture map). If it fails, the geometry is wrong — the fix is never
  a fixture or structural check (L8).

### Mandated correctness oracle

The executor's own unit tests are necessary but not sufficient; the failure class that matters
here is silent (plausible-looking visibility grids that violate the rules). Two instruments:

1. **Independent dense-ray sampler (the oracle).** In a script (not shipped code), implement a
   brute-force reference: for each tile in the window, cast many continuous rays from the origin
   point to sample points spread over the target tile's square (at minimum: center, four inset
   corners, edge midpoints), marching each ray through the grid exactly (a supercover / DDA walk
   over cell boundaries), applying the same opacity and budget rules per L2/L3. A tile is
   oracle-visible at cost `c` if any sampled ray reaches it having passed through exactly `c`
   translucent tiles with `c` within budget; the minimum such `c` is the oracle cost.
   - **Hard rule:** every oracle-visible tile must be engine-visible with engine cost ≤ oracle
     cost. A sampled ray is a constructive witness; an engine miss here is a soundness bug, no
     exceptions.
   - **Soft rule:** engine-visible tiles the oracle missed are either under-sampling (increase
     samples; the discrepancy should shrink) or genuine over-permissiveness (a bug). Triage each
     class manually and record the outcome in this document.
   - **The oracle may NOT be compared against the old engine, and may NOT be edited to make the
     new engine pass.** If oracle and engine disagree after triage, the disagreement gets written
     up here and decided, not patched around.
   Run the oracle over the existing characterization maps plus at least 50 seeded random maps
   (mixed wall/glass density, budgets 0, 1, 2, 255) and window sizes on both sides of practical
   viewport size.

2. **Characterization suite.** All existing `fov-tests.fsx` cases pass unmodified, plus the Stage 1
   additions (bracketed doorways, shallow-angle glass, multi-pane, large windows, corner-touch
   from T4). If a new test contradicts a declared behavior change above, the test is asserting the
   old engine's artifact — bring it here for a decision instead of weakening the engine.

### Verification-before-timing contract

Benchmarks run only after the oracle and suite are green — never tune while red. Gate for merge:
`fov-bench.fsx` at the four standard windows (1080p-ish, 1440p-ish, 4k-ish, stress) within **2×**
of the 2026-07-03 snapshot recorded above. If the clean implementation misses the gate, the first
suspects are per-compute allocation and interval representation (L4/L6), not the visibility rules
— the algorithmic contract does not bend for the benchmark. Record before/after numbers in this
document's History.

### Cleanup addendum (extends Stage 5)

When the old engine is deleted, also remove `postPassStamp`, `postPassCost`, `BeginPostPass`, and
the post-pass tick from `VisibilityState` — they exist only for the crack-fill pass and become dead
state. Replace them with whatever preallocated interval buffers the new engine needs. The
`enableDiagonalTouchMarking` / `enableNonOpaquePinholeFill` flags and their code move to `archive/`
or die entirely; they must not remain as dormant branches.

## Acceptance Criteria

The rewrite is successful when the live FOV code has one primary visibility algorithm, no permanently disabled smoothing paths, and no final cleanup pass that can reveal tiles without going through the same opacity and translucency rules as the main path.

The tests should pass for both sides of the old artifact pressure: tiles behind glass or beside doors should not appear too early, and visible wall or glass faces should not disappear just because no perimeter ray hit their center. The bookshelf-behind-wall regression should stay in the suite because it proved that fixture opacity and structural wall-face visibility must not be conflated.

The code should make ownership obvious. `TileMap` owns effective opacity and exploration. `VisibilityState` owns current visibility and translucency depth. Geometry helpers own angular or slope math. The FOV algorithm coordinates those pieces, but should not also contain a graveyard of unused experiments.

The performance target is to stay in the same practical range as the current implementation for normal viewport sizes. Exact timings can move, especially during the first clean implementation, but the system should remain cheap enough to recompute on ordinary player movement without renderer involvement.

## Implementation Notes And Deviations

The interval implementation follows the tier-0 behavior contract, but a few execution details are worth preserving for future review.

The one literal deviation from L7 is explored marking. `RectFov.compute` does not call `TileMap.MarkExplored` from the hot path; `stampVisible` writes `map.Explored.[idx] <- 1uy` directly when the chosen cost is `0`. The semantic rule is unchanged: only cost-`0` visibility explores, and the loops have already clipped coordinates to map bounds. This was kept because the explored write sits in the innermost stamping path. If `MarkExplored` later gains side effects beyond a bounds-checked byte write, either route this helper through that API again or update this note and the helper together.

The L4 exact-arithmetic requirement is implemented, but not as a named "double all corner coordinates" helper. Per-octant tile spans use the equivalent rational slope formulas `(2 * minor - 1) / (2 * depth + 1)` and `(2 * minor + 1) / (2 * depth - 1)`, with explicit seam clamps to `0` and `1`. Slope comparisons still use integer cross-multiplication with `int64`; there is no floating point angle and no epsilon.

The optional L6 debug assertion for cost being non-decreasing along a fixed angle was not added. The invariant is enforced by construction through sorted, disjoint interval lists and `applyOccluderSpan`, and the cheap assertion shape was not obvious without adding noisy hot-path scanning. Keep future checks focused on interval-list invariants or on oracle coverage rather than adding a broad per-tile assertion to compute.

The oracle is intentionally a hard witness checker, not an equivalence proof. `fov-oracle.fsx` samples a `7x7` inset grid inside each target tile, which is stronger than the mandated minimum of center, corners, and edge midpoints. It also treats exact grid-corner ray crossings as non-witnesses when either side cell blocks, matching the positive-measure corner policy. A hard oracle miss is a correctness bug. A soft engine-only tile may still be valid positive-measure visibility that finite sampling missed.

The main optimization challenge was that the clean interval engine was correct before it was comfortably within the benchmark gate. Useful optimizations were rectangular per-octant depth clipping, per-octant minor-loop clipping, replacing octant `match` work with direction constants and incremental coordinates, using `int` slope numerators and denominators with `int64` comparisons, simplifying the minimum-cost overlap scan, preallocating interval buffers on `VisibilityState`, and writing exploration directly in `stampVisible`. One attempted optimization was backed out: preserving an interval scan hint across occluder updates made the benchmark worse, so the implementation resets the hint after interval-list mutation. The stress benchmark remains the closest line to the `2x` gate; future tuning should target interval fragmentation and scan behavior, not reintroduce repair passes or structural special-casing.

Two concrete tuning candidates from the tier-0 review gate, in priority order, should the stress margin ever need to shrink:

- **Free win:** `minOverlappingCostFrom` has no early exit when it finds a cost-`0` interval. No lower cost exists, so the scan can stop immediately. On dense translucent maps (the stress benchmark's glass stripes) that scan is hot and frequently finds cost `0` early. Semantics-free.
- **The structural lever:** the hint reset after every occluder mutation means dense maps restart the interval scan from index `0` on almost every tile. The backed-out attempt preserved the *old* list's hint, which is why it lost; the correct variant carries the hint *through* `applyOccluderSpan` by recording the output index of the first appended interval at or after the occluded span. Tile spans are monotone in the minor axis, so that output index is a valid scan floor for the next tile. This restores monotone scanning on exactly the maps where the reset hurts most. Only worth doing with the benchmark as referee.

No `GetTileProperties` or structural/fixture distinction remains in `RectFov.compute`. That is intentional, not an omission. The old distinction existed to restrain face-fill repair logic; with the interval engine, effective opacity is the only visibility input.

## Limitations

`fov-tests.fsx` now pins down the core FOV contract: translucent tiles are visible before they spend budget, tiles behind glass carry translucency depth without being explored, opaque faces block, zero-budget glass should not leak into side pockets, shallow-angle glass faces next to visible floor should still be shown, opaque fixtures behind a wall next to a doorway should stay hidden, direct opaque fixtures in open sight should be visible and blocking, corner-touching walls should not leak sight, translucent seam tiles should not double-spend budget, and large windows should use the same visibility rules as small windows.

`fov-oracle.fsx` is a dense sampled witness oracle, not a proof of exact equivalence. Its hard rule is authoritative: every sampled oracle-visible tile must be visible in the engine at no worse cost. Its soft reports are engine-visible tiles missed by finite sampling. The current soft class is dominated by narrow oblique wall/glass slivers that are expected from positive-measure square spans and were not treated as merge blockers after the hard oracle passed.

`VisibilityState` is fixed-size. If the map dimensions change, create a matching new state instead of reusing the old one.

Current visibility is not serialized. Only exploration lives on `TileMap` and participates in map persistence.

## Source Map

- `FOV.fs` - `VisibilityState`, exact slope intervals, `RectFov.compute`, query helpers, interval occlusion, and explored marking.
- `Maps.fs` - runtime `TileMap`, effective opacity cache, `Explored`, `MarkExplored`, `IsExplored`, and `ClearExplored`.
- `Types.fs` - `TileOpacity`.
- `GameState.fs` - `GameUpdate.recomputeVisibility`.
- `fov-tests.fsx` - characterization tests for translucency, exploration, blocking, and the known over-permissive/over-strict artifact risks.
- `fov-oracle.fsx` - independent dense sampled oracle for hard witness checks and soft over-visibility triage, plus the window-clip differential check that pins the rectangular clipping path.
- `fov-bench.fsx` - current benchmark script and older inline timing notes.
- `archive/fov-gpt5.2-alt.txt` - archived alternate FOV implementation.
- `archive/fov-opus-alt.txt` - archived alternate FOV implementation.
- `archive/fov-simpler-alt.txt` - archived simpler FOV implementation.

## History

### 2026-07-03 (tier-0 review gate — implementation verdict)

Gate verdict: **PASS**. Reviewed `FOV.fs` against the work order: L1–L8 implemented as written.
Verified in code: full-square spans with exact rational slope pairs and `int64` cross-multiplication
(L1/L4), strict-inequality overlap test and zero-width interval discard (L2/T4), pre-spend stamping
of translucent tiles (T3), origin never spends budget (L3), per-octant column-outward ordering with
per-octant interval state (L5/T2/T5), sorted/disjoint/merged preallocated buffers (L6), unchanged
public queries and `translucencyDepth` presentation rule (L7), and no repair passes or
structural/fixture distinction anywhere in compute (L8).

Additional evidence gathered at the gate, beyond the executor's own verification:

- **Soft-miss triage completed with the mandated discriminator.** A denser sampler variant
  (121 samples/tile, insets to 1/64 instead of 2/16) run over all 58 oracle cases produced zero
  hard failures and **zero soft misses**. Every soft miss in the standard run was finite sampling
  of genuine edge-sliver visibility (the declared oblique-face behavior change), not
  over-permissiveness. The soft class is closed.
- **Window-clip differential check.** The oracle only exercises full-map windows, leaving the
  rectangular clipping path untested. Property checked: clipped-window visibility must equal
  full-map visibility intersected with the window (window convexity guarantees this). 20 seeded
  random maps, 29,116 tile comparisons: 0 mismatches. Folded into `fov-oracle.fsx` as a permanent
  section (2026-07-03), so the clipping path stays pinned on every oracle run.
- **Independent benchmark run** vs the pre-rewrite snapshot (gate ≤ 2×): 1080p-ish 0.3048 ms
  (1.09×), 1440p-ish 0.4222 ms (0.67×), 4k-ish 0.8496 ms (0.88×), stress 3.2364 ms (1.62×).
  Gate met with margin; two sizes are faster than the old engine.

One behavior change not recorded in the executor's deviation notes, recorded here as intended:
the old engine stopped rays immediately when the origin tile itself was opaque (only the origin
visible); the interval engine ignores origin-tile opacity entirely. Standing inside an opaque tile
is not a supported game state, so this is accepted as-is.

The `MarkExplored` bypass, the tile-span formulation of L4, and the omitted L6 debug assertion are
documented in Implementation Notes And Deviations above and are accepted.

### 2026-07-03 (interval engine implementation)

- Replaced the old perimeter-ray plus repair-pass implementation with the interval visibility engine described above.
- Removed the old repair stack from live compute: adaptive refinement, large-window crack fill, structural wall/glass face fill, transparent bracket fill, disabled diagonal touch marking, disabled non-opaque pinhole fill, and `VisibilityState` post-pass scratch arrays.
- Added Stage 1 characterization tests for bracketed doorways, direct opaque fixtures, multi-pane translucency depth, corner-touch blocking, seam translucency budget, and large-window consistency.
- Added `fov-oracle.fsx`, a dense sampled DDA witness oracle over characterization maps, one large-window map, and 50 seeded random maps across budgets `0`, `1`, `2`, and `255`.
- Oracle result: hard witness checks passed. Soft reports remain for narrow oblique square-span slivers that the finite sampler misses; these are documented as expected positive-measure interval visibility rather than merge blockers.
- Final benchmark verification range after hot-path cleanup:

```text
1080p-ish: 0.2190-0.3243 ms/compute
1440p-ish: 0.3724-0.5909 ms/compute
4k-ish:    1.1943-1.7373 ms/compute
stress:    3.3101-3.9818 ms/compute
```

- Verified with `dotnet build -v:minimal`, `dotnet fsi .\fov-tests.fsx`, `dotnet fsi .\fov-oracle.fsx`, `dotnet fsi .\fov-bench.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\map-tests.fsx`, and `dotnet fsi .\entity-registry-test.fsx`.

### 2026-07-03 (tier-0 review)

- Tier-0 review of the rewrite plan: interval-based direction approved; added the execution work
  order with locked decisions L1–L8, declared behavior changes, enumerated traps T1–T6, the
  mandated dense-ray-sampler oracle, and the verification-before-timing benchmark gate.
- Corrected Stage 3: the migration oracle is the independent sampler plus the characterization
  suite, not stamp-for-stamp equality with the old engine (whose output includes the fill
  artifacts the rewrite removes).
- Verified against the current `FOV.fs` that the bookshelf regression holds under pure geometry
  (no straight sightline reaches the fixture through the doorway), so the structural/fixture
  distinction can be deleted from compute entirely (L8).

### 2026-07-03

- Added the first live fog-of-war system document.
- Captured the current contract: rectangular current visibility, map-owned exploration, and translucency cost as part of visibility rather than a rendering afterthought.
- Recorded the current v0.2d performance shape and the distinction between active behavior and archived/benchmark experiment labels.
- Clarified that the disabled smoothing toggles are inactive because the present setup worked best, not because the translucency artifact problem was fully solved.
- Added focused `fov-tests.fsx` characterization coverage for the core translucency contract and the two artifact pressures: side tiles becoming visible too early and glass faces disappearing at awkward angles.
- Added a doorway-adjacent fixture regression: wall/glass face filling now applies only to structural base tiles, so an opaque bookshelf behind a wall next to an open doorway is not revealed early by the cleanup pass.
- Added a staged plan for replacing perimeter rays plus cleanup passes with an interval-based visibility algorithm, including contract tests, geometry helpers, migration rules, inactive-path cleanup, benchmark expectations, and acceptance criteria.
