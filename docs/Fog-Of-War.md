# Fog Of War And Visibility

This document explains how the current fog-of-war and field-of-view system works. The implementation lives in `FOV.fs`.

The short version: this is not ordinary binary fog of war. Each compute step finds volume-visible tiles inside a rectangular view, remembers a translucency depth for those tiles, presents directly seen wall and corner surfaces, and marks directly seen tiles or directly presented surfaces as permanently explored on the `TileMap`. Translucent tiles such as glass are visible themselves, but they spend a limited budget for the tiles behind them.

## Why This Is Not Regular FOV

Regular tile FOV usually answers one question: "is this tile visible from the player?" This system has to answer several related questions without letting one answer corrupt the others.

The first question is volume visibility: can sight enter this tile's square with positive angular width? That is the propagation answer used by `RectFov.isVisible`, the oracle, and the translucency-cost layer.

The second question is surface presentation: can the player directly see the face or corner surface of a non-open tile even if sight cannot enter its volume, or even if a glass sliver also reaches it at a worse depth? That is handled by the non-propagating surface pass and exposed through `RectFov.isPresentedVisible` and `RectFov.presentedDepth`.

The third question is translucency depth: how many translucent cells did the current volume path pass through before reaching this tile? A translucent tile is visible before it spends budget; the cells behind it pay the added cost. This is why window behavior is harder than ordinary occlusion: the glass tile itself should be clear, the space behind it should be visible only within budget and rendered dimmer, and nearby wall or fixture surfaces should not pop in or disappear because of a narrow angular artifact.

The fourth question is exploration: should this tile become permanently known on the `TileMap`? Current visibility is temporary, but exploration persists. Clear cost-`0` volume visibility explores tiles, and direct surface presentation explores surfaces; through-glass volume visibility alone does not.

Keeping those answers separate is the main design constraint. `RectFov.isVisible` remains volume-only so geometry stays testable. Presentation queries combine volume visibility with surface presentation for rendering. Translucency depth remains pure volume cost, while presented depth chooses direct surface depth `0` when a surface is directly presented.

## Explanation

The live system is built around `RectFov.compute`. The caller gives it a runtime `TileMap`, a reusable `VisibilityState`, an origin position, a rectangular half-width and half-height, and a translucency budget.

The result has four related answers.

Volume visibility is temporary. A tile is volume-visible when its entry in `visibleStamp` was stamped during the latest compute step. This is the propagation layer and the value returned by `RectFov.isVisible`.

Surface presentation is also temporary. A non-open tile is surface-presented when `disclosedStamp` was stamped by the surface pass. Public `RectFov.isDisclosed` reports only surface-presented cells that are not also volume-visible, while `RectFov.isPresentedVisible` reports volume-visible or surface-presented.

Translucency depth belongs to volume visibility. Visible cells carry a cost that means "how many translucent cells did the ray already pass through before reaching this cell?" A cost of `0` is fully clear visibility. A cost greater than `0` means the tile is visible only through one or more translucent cells.

Exploration is persistent. A tile is explored when `TileMap.Explored` has been marked. Exploration survives later visibility recomputes until the map's explored grid is explicitly cleared.

## Data It Owns

`VisibilityState` owns reusable arrays sized to the map:

- `visibleStamp`: the current-visibility stamp for each cell.
- `disclosedStamp`: the current surface-presentation stamp for wall/corner presentation. Public `isDisclosed` reports only stamped cells that are not also volume-visible, but the same stamp can also override presented depth for a volume-visible surface.
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

There are no perimeter rays, adaptive refinement rays, large-window crack fill, transparent bracket fill, structural-tile checks, or disabled smoothing toggles in the live volume path. The interval output is final for propagation and for `RectFov.isVisible`.

After all eight octants finish, `RectFov.compute` runs a surface-presentation post-pass over the clipped window. This pass does not mutate volume visibility, translucency cost, or interval state. It can stamp `disclosedStamp` for an opaque, translucent, or unknown-opacity tile that is hidden from volume FOV, or for one that is volume-visible only at translucency cost `>= 1`. Volume-visible cost-`0` cells are already direct and are skipped. A candidate needs at least one origin-facing neighbor that is a cost-`0` volume-visible Transparent or Air cell. Non-origin neighbors must also pass the boundary-support ray: the straight segment from the origin center to the part of that neighbor directly in front of the candidate surface must cross only Transparent or Air cells. The origin tile itself qualifies as a discloser without a support ray. Stamped surfaces become presentation-visible at depth `0` and become explored, but they never propagate sight.

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

Surface presentation extends that rule only for direct surfaces. A stamped surface is marked explored because presentation requires a cost-`0` transparent or air neighbor facing the origin, or origin adjacency. Tiles seen only through glass without direct surface support are still not explored by this rule.

`RectFov.translucencyDepth` reports the current volume translucency cost for volume-visible tiles. It does not flatten explored opaque or translucent cells to depth `0`; a previously explored wall, pillar, or bookshelf currently seen through glass still reports the through-glass cost. Out-of-bounds or not-currently-visible cells return `-1`.

For renderer-facing presentation, use `RectFov.isPresentedVisible` and `RectFov.presentedDepth`. Presented visibility means volume-visible or surface-presented. Presented depth is `0` for surface-presented tiles, otherwise the volume translucency depth for volume-visible tiles, otherwise `-1`.

## How It Fits The Game

`GameModel.VisibilityState` is a derived cache. `GameUpdate.recomputeVisibility` is the only engine entry point that refreshes it. It passes the model's map, visibility state, player position, rectangular view size, and translucency budget into `RectFov.compute`.

Movement, door interactions, pushes, window changes, and translucency-budget changes do not refresh this cache automatically. They report visibility-impacting mutations through `Changes.VisibilityInputChanged`, or update the model fields that the next compute will use. Between explicit recomputes, `VisibilityState` can be empty or stale by design. A consumer that reads current visibility must own the recompute cadence and call `GameUpdate.recomputeVisibility` before reading when its cadence requires fresh data.

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

Surface-disclosure verification range from two 2026-07-04 runs, with disclosure included in timed compute:

```text
1080p-ish: 0.3246-0.3775 ms/compute (half=20x10)
1440p-ish: 0.5193-0.6573 ms/compute (half=26x13)
4k-ish:    1.1284-1.2260 ms/compute (half=40x20)
stress:    3.3672-3.9271 ms/compute (half=60x40)
```

This remains inside the same `2x` gate. The stress case is still the closest line to the gate.

## Tradeoffs

The algorithm favors a rectangular viewport contract. This fits the current game update path and keeps work proportional to the area around the player, but it is not a general-purpose circular shadowcasting API.

The translucent budget is a gameplay/display concept, not physically correct light transport. It gives the renderer or game layer enough information to distinguish clear visibility from visibility through glass-like material, while keeping the compute path cheap.

The interval engine is more explicit than the old perimeter-ray implementation. It removes pinholes and fill artifacts by making the tile square's angular span the source of truth, but it does more interval bookkeeping than a simple ray perimeter. The hot path keeps reusable interval arrays on `VisibilityState`, clips each octant to the rectangular viewport, and keeps slope comparisons exact with integer cross-multiplication.

The new output intentionally differs from the old heuristic fills in some places. Oblique wall and glass faces can become visible when their square span has genuine overlap with the open interval. Doorway tiles are visible because their spans overlap, not because they are bracketed by walls. Tiles that were visible only because of old fill repairs may disappear if no positive-measure interval reaches them.

The implementation reads only effective opacity. It does not distinguish structural base tiles from opaque fixtures in `RectFov.compute`; that distinction existed only to keep the old face-fill pass from revealing fixtures too early. Surface disclosure avoids the old bookshelf regression by using only origin-facing cost-`0` transparent or air neighbors as disclosers, not by asking whether a tile is "structural."

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

The cost-`0` early-exit tuning candidate from the tier-0 review gate landed with the boundary-supported disclosure implementation. `minOverlappingCostFrom` now stops scanning as soon as it finds cost `0`, because no lower cost exists. This was semantics-free and helped keep the stress benchmark comfortably inside the gate after support rays entered the disclosure pass.

One remaining structural tuning candidate should the stress margin ever need to shrink further: the hint reset after every occluder mutation means dense maps restart the interval scan from index `0` on almost every tile. The backed-out attempt preserved the *old* list's hint, which is why it lost; the correct variant carries the hint *through* `applyOccluderSpan` by recording the output index of the first appended interval at or after the occluded span. Tile spans are monotone in the minor axis, so that output index is a valid scan floor for the next tile. This restores monotone scanning on exactly the maps where the reset hurts most. Only worth doing with the benchmark as referee.

No `GetTileProperties` or structural/fixture distinction remains in `RectFov.compute`. That is intentional, not an omission. The old distinction existed to restrain face-fill repair logic; with the interval engine, effective opacity is the only visibility input.

## Current Visual Issues And Proposed Fix

The 2026-07-04 playtest screenshots show that the interval engine solved the old repair-pass mess, but it also exposed places where the current output contract is too coarse for the desired presentation.

The first issue is over-clear visibility through windows. A translucent tile is currently treated as a full-square pass-through cell. Once an angular interval survives through that tile, any later tile with positive-measure overlap can become visible with the same binary current-visibility stamp as ordinary direct sight, only with a higher translucency cost. In the screenshots, this makes the tiles diagonally above the window and distant pillars appear perfectly clear when they should be weak, partial glimpses through a small aperture.

The second issue is missing near-field visibility. Tiles close to the player, especially the top-left and top-right neighboring areas in the second and third screenshots, can be marked explored but not currently visible. That is a bad fit for what the player sees on screen: adjacent floor, wall faces, and corner surfaces should not remain dark merely because the volume-based interval test did not enter that tile's square with positive measure in the right octant.

The third issue is permanently black or permanently dark corners. Some corner cells never become explored at all; others become explored but are never lit as currently visible from positions where their wall face or corner surface is plainly in view. This suggests that the current algorithm is answering only "can sight enter this tile volume?" while the renderer also needs "can the player see this boundary surface?"

The proposed fix is not to restore the old repair passes. The cleaner direction is to split FOV output into a few explicit layers:

- **Volume visibility:** keep the interval engine as the primary answer for open tile volumes and propagation. This layer controls whether sight can continue through transparent, translucent, or opaque cells.
- **Local visibility baseline:** resolve the origin's immediate neighborhood with a small non-propagating rule. Adjacent transparent cells and directly adjacent visible wall or corner surfaces should be currently visible unless an opaque cell actually blocks that local view. This should fix near-player explored-but-dark artifacts without creating new sight behind the tile.
- **Surface disclosure:** add a principled wall/corner face rule that reveals surfaces adjacent to visible volume. This layer marks the face or owning tile visible for presentation, and possibly explored if it is a direct cost-`0` surface, but it never opens a new angular interval and never reveals opaque fixtures behind walls.
- **Aperture transmission for translucent tiles:** stop treating every translucent tile as a full-square window. A window, porthole, grate, or glass slit should transmit only through an aperture span smaller than the whole tile, while the translucent tile itself remains visible at the pre-spend cost. That should reduce the side-tile and far-pillar over-visibility without special-casing those maps.
- **Visibility strength or coverage:** keep enough information to distinguish direct sight, narrow partial sight, and sight through one or more translucent cells. A tile seen through a tiny window span should not render as clearly as direct floor. Opaque objects may need a higher coverage threshold than floor or wall surfaces before they are presented as identified.
- **Explicit exploration semantics:** decide whether translucent glimpses explore nothing, explore weakly, or explore only surfaces. The current rule, "only cost `0` stamps explored," is simple, but the screenshots show that presentation and exploration may need separate states once surface disclosure and aperture visibility exist.

This keeps the codebase direction clean. The interval engine remains the propagation core; the new layers add missing presentation semantics around local sight, wall surfaces, and window apertures. Each layer should be testable on its own and should have a non-propagation rule where appropriate, so the fix does not become a new set of hidden bracket-fill or face-fill hacks.

The tier-0 verdicts on this layer list and the locked execution contract are in the work order below. Where the two differ, the work order governs.

## Tier-0 Work Order: Surface Disclosure And Window Presentation (2026-07-04)

Tier-0 verdict on the visual issues and the proposed layer list above: **diagnosis confirmed, scope
cut**. The six proposed layers reduce to one new engine mechanism (surface disclosure), one staged
mechanism (coverage), one frontend obligation (depth dimming), and one minimal exploration
amendment. Two proposed layers are rejected or deferred, with reasons below.

Same contract as the 2026-07-03 work order: a tier-1 executor implements from this document and the
repo alone; **locked** decisions are implement-as-written; if a locked decision appears wrong during
execution, stop and report. Numbering continues the existing series (L9+, T7+).

### Verified diagnosis

The reported artifacts were reproduced in isolation before judging the fix. Findings:

- **Room corners are never visible and never explored.** In a plain rectangular room, all four
  corner wall tiles stay permanently black from every interior position, including standing
  directly adjacent. This is *exact* L1 behavior, not an engine bug: a corner tile bracketed by two
  opaque orthogonal neighbors has measure-zero visibility from a point origin (the only ray is the
  exact diagonal through the shared corner point). No volume-visibility tuning can fix this;
  presentation must.
- **Wall runs are fine.** A long wall seen obliquely lights up along its whole length (each tile
  keeps a positive-measure sliver under the run's face). The near-field problem is exclusively
  corners and fully-bracketed faces, not runs. "Adjacent floor" in the issue text is a
  misattribution: orthogonally adjacent open tiles are *provably always* volume-visible (nothing
  exists at depth < 1 to occlude them), and diagonally adjacent open tiles are dark only when both
  bracketing orthogonals are opaque — i.e. around a solid corner, where dark is *correct* (lighting
  that tile would reveal room contents around the corner).
- **Window over-clarity is a presentation gap, not an engine gap.** Tiles behind glass carry a
  distinct translucency cost (`GetTranslucencyCost` / `translucencyDepth`), but nothing consumes it
  for dimming — the engine repo has no renderer, and the binary `isVisible` stamp is all the
  frontend currently appears to use. Behind a single pane, an entire back room stamps at cost 1;
  rendered undimmed, that reads as perfectly clear sight.

### Verdicts on the proposed layers

1. **Volume visibility** — keep, frozen. The interval engine and its oracle are untouched.
2. **Local visibility baseline** — **rejected as a separate mechanism**, folded into surface
   disclosure. Orthogonal open neighbors never need it; diagonal open neighbors must not get it
   (around-corner leak). What remains is exactly wall/corner surfaces, which disclosure covers.
3. **Surface disclosure** — **approved**, locked as L10–L14 below.
4. **Aperture transmission** — **deferred entirely.** It is a content feature (needs per-tile
   aperture data), it changes propagation geometry (the DDA oracle models full-square glass and
   would need a rework), and it interacts with budget semantics. Revisit only if depth dimming plus
   coverage prove insufficient in playtest, and then only via a new tier-0 work order.
5. **Visibility strength / coverage** — **staged** (L15). Depth dimming first; coverage only on
   playtest evidence that dimming alone cannot separate a wide window view from a narrow glimpse.
6. **Explicit exploration semantics** — **resolved minimally** (L13). Disclosed surfaces explore;
   nothing else about exploration changes. No weak-explore states.

### Locked decisions

**L9 — The volume layer is frozen; disclosure is a separate output, not a repair.**
L1–L8 continue to govern `RectFov.compute`'s interval propagation unchanged. Surface disclosure
never mutates the visibility stamp, the cost array, or interval state, never opens or widens an
angular interval, and never propagates. L8's "no compensating passes" rule stands for the volume
layer; disclosure is permitted because it is a *named, separately queryable presentation layer*
with its own stamp — not a mutation of volume output. If an implementation of disclosure ever needs
to write `visibleStamp` or `translucencyCost`, it is wrong; stop.

**L10 — The disclosure rule (origin-facing neighbors).**
A tile is a disclosure *candidate* iff it is inside the clipped window, is not volume-visible this
tick, and its effective opacity is not Transparent or Air (i.e. Opaque, Translucent, or unknown —
same conservatism as L2). For a candidate at `(x, y)` with origin `(ox, oy)`, let
`sx = sign(ox - x)` and `sy = sign(oy - y)`. The candidate is **disclosed** iff at least one of its
origin-facing neighbors `(x + sx, y)`, `(x, y + sy)`, `(x + sx, y + sy)` (skipping coordinates that
duplicate the candidate when `sx = 0` or `sy = 0`) is in bounds, volume-visible at cost `0` this
tick, and effective-Transparent or Air. The origin tile itself qualifies as a discloser (it is
always stamped cost `0`). The direction constraint is load-bearing — see T7.

**L11 — Disclosure runs as a post-pass inside `compute`.**
It runs once, after all eight octants finish, scanning only the clipped window rectangle. It must
not run per-octant or interleaved with stamping — volume cost-0 stamps are not final until all
octants complete (a discloser can gain its stamp in a later octant), and interleaving makes output
order-dependent. `GameUpdate.recomputeVisibility` keeps its single `RectFov.compute` call.

**L12 — Output contract.**
Disclosure gets its own tick-based stamp on `VisibilityState` (same tick discipline as the
visibility stamp; preallocated; zero steady-state allocation). `isVisible` stays volume-only — it
is the oracle target and must not absorb disclosure. Add a presentation query for the frontend:
presented-visible = volume-visible OR disclosed, and presented depth = volume `translucencyDepth`
for volume-visible tiles, `0` for disclosed-only tiles (direct surface sight). Exact member names
are the executor's choice; these semantics are not.

**L13 — Exploration amendment (extends L7's rule).**
A disclosed tile is marked explored. This is safe because L10 requires a cost-`0` discloser, so
disclosure is always direct surface sight — it is the "room corners never become explored" fix.
Everything else is unchanged: volume visibility at cost ≥ 1 still never explores, and glimpses
through glass still disclose nothing (a glass discloser fails L10's effective-Transparent test).

**L14 — Open tiles are never disclosed.**
Transparent and Air tiles are never disclosure candidates. The dark diagonal floor tile around a
solid corner stays dark: it is not a presentation artifact, it is correct occlusion, and force
lighting it leaks whatever stands there. This is the rejection of the "local visibility baseline"
layer, locked so it is not reintroduced under another name.

**L15 — Window over-clarity is fixed in stages, presentation-first.**
Stage A requires **no engine change**: the frontend dims by presented depth (cost ≥ 1 renders
attenuated, not clear). Stage B, only if playtest after Stage A still cannot distinguish a wide
window view from a narrow glimpse: add an advisory per-tile *coverage* value — the fraction of the
tile's angular span overlapped by surviving intervals, accumulated during the existing min-cost
scan. Coverage is presentation data: approximate arithmetic (float or fixed-point) is permitted
there — L4 exactness governs occlusion decisions only — and coverage must never feed back into
interval or occlusion decisions, must clamp to `[0, 1]`, and is ignored by the oracle. Aperture
transmission stays deferred per verdict 4 regardless of Stage B.

### Enumerated traps

- **T7 — Naive adjacency re-reveals the T6 bookshelf.** In the T6 map the bookshelf is 8-adjacent
  to the visible open doorway, so a disclosure rule without L10's direction constraint lights it —
  the old face-fill bug reborn. With the constraint it stays hidden: all three of its origin-facing
  neighbors are walls. Extend the existing T6 test to assert the bookshelf is not disclosed, not
  presented, and not explored.
- **T8 — The local-baseline temptation.** Any "adjacent tiles should just be visible" shortcut
  reveals around solid corners. Rejected by L14; do not implement it as a fallback, a debug toggle,
  or a special case for the origin's ring.
- **T9 — Disclosure from non-final volume state.** Running disclosure per-octant reads cost-0
  stamps that may only appear in a later octant, making output depend on octant order. Post-pass
  only (L11).
- **T10 — Disclosure leaking into volume state.** Disclosure writes its own stamp and `Explored`,
  nothing else. In particular it must not make `isVisible` true (L12) — the oracle and the
  window-clip differential check both depend on `isVisible` staying pure volume output.
- **T11 — The clip property extends to disclosure.** Every origin-facing neighbor of an in-window
  tile is itself in-window (each step moves toward the origin inside an axis-aligned rectangle
  containing it), so clipped-window disclosure must equal full-map disclosure intersected with the
  window. Extend the oracle's window-clip differential section to compare disclosure stamps too;
  any mismatch is fatal.
- **T12 — Coverage double-counting on seams (Stage B only).** Seam tiles are visited by two
  octants with disjoint clamped half-spans; summing their contributions is correct, but the sum
  must clamp to full coverage, and a fully open map must report coverage 1 for every visible tile.
  Add that as a test before trusting coverage for rendering.

### Oracle scope for this work order

The dense-ray sampler stays the authority for the volume layer only, unchanged. Disclosure is a
presentation heuristic, not geometry — a sampled ray oracle *cannot* witness it, and the oracle
must not be extended or edited to try. The only mandated oracle change is T11's extension of the
window-clip differential section. Disclosure correctness is pinned by characterization tests:

- Room corners: all four disclosed, presented, and explored from an interior position (and from a
  directly adjacent position).
- Around-corner diagonal floor: stays dark and undisclosed (L14).
- T6 bookshelf: hidden, undisclosed, unexplored (T7).
- Glass discloser at budget 0: a wall whose only cost-0 visible neighbor is glass is not disclosed.
- Wall directly behind a wall: not disclosed (its only origin-facing neighbor is the near wall).

### Verification-before-timing contract

Same discipline as the interval work order. All existing tests, the new disclosure tests, and the
full oracle (including the extended clip section) must pass **before** any benchmark tuning. Then
the same gate: no more than `2x` of the 2026-07-03 pre-rewrite snapshot on every benchmark size,
disclosure pass included in the timed compute. The `minOverlappingCostFrom` cost-`0` early exit has
already landed; if the margin erodes again, the remaining lever is carrying the interval scan hint
through `applyOccluderSpan`, as described under Implementation Notes And Deviations. Do not weaken
disclosure semantics for speed.

Frontend follow-up after the engine lands: render lit from the presented query, dim by presented
depth, and bring back playtest screenshots. The Stage B coverage decision waits on that evidence.

### Amendment (2026-07-04, post-playtest): through-glass presented depth

Playtest after the disclosure landing showed one remaining artifact: explored walls and
effectively-opaque fixtures (far-corner pillars, the bookshelf) render crystal clear through windows
while floors behind the same glass dim correctly. Diagnosis is confirmed engine-side, resolving the
diagnostic fork recorded in the History note: the object anchor tiles report presented depth `0`.

Root cause: `RectFov.translucencyDepth` carries a legacy explored-override inherited verbatim from
the pre-rewrite FOV — if the queried tile is volume-visible, explored, and its **effective** opacity
is Opaque or Translucent, the query returns depth `0` instead of the volume cost.
`TileMap.GetOpacity(x, y)` reads the effective-opacity cache, so opaque fixtures (pillars,
bookshelves) take the same branch as walls. Floors take the `_ -> cost` branch, which is why the
floor dims and the objects don't. The frontend is not at fault: `GameSpriteOverlay.cs` consumes
`presentedDepth` correctly; it is being fed `0`.

Empirical confirmation (two-compute sequence, mirroring real play): explore a room from inside, then
view it through glass — the far wall reports `volumeCost=1, presentedDepth=0, explored=true`; the
identical view with the room never explored reports `presentedDepth=1`. Floors report `1` in both.

**L16 — presented depth is pure volume depth.** Delete the explored-override in
`RectFov.translucencyDepth`: when volume-visible, return the volume translucency cost unmodified.
Presented depth then has exactly the two cases L12 locked — volume cost when volume-visible, `0`
when disclosed-only — with no third case keyed on exploration or opacity. Memory-brightening of
explored tiles seen through glass is frontend policy, and the frontend already owns the knob for it
(`FogTranslucentExploredAlphaFactor` in `GameSpriteOverlay.cs`); the engine must report geometric
truth. No frontend change is required for this fix.

**T13 — the override is pinned by no test, which is exactly why it survived two review gates.** In
any single-compute test, a tile is explored only if it was stamped at cost `0` (or disclosed, which
is not volume-visible), so the override branch never changes an assertion. It only fires when
exploration history persists across moves. Mandated regression test (two computes on one map/state):
explore a room from inside at cost `0`; recompute from behind glass; assert the explored
effectively-opaque tile seen through glass has `translucencyDepth = presentedDepth =` its volume
cost (`>= 1`); assert an explored floor behind the same glass also reports its volume cost; assert
the directly-seen glass pane stays `0`. Use an opaque *fixture* (not just a wall tile) for at least
one asserted tile so the effective-opacity path is covered.

**T14 — query-only change; do not touch the volume layer or the oracle.** `RectFov.compute`, the
stamps, and the cost array are untouched by this fix. The dense oracle and the window-clip
differential compare stamps and costs, not this query — do not extend the oracle to "cover" the fix;
the mandated characterization test is the pin. The benchmark gate is unaffected (the query is not on
the timed path); re-running the bench is optional confirmation, not license to tune.

### Open Review (2026-07-04): end-of-wall corner peeking

Post-L16 playtest found a separate issue that is not caused by glass. Repro sketch:

```text
P . . . . .
# # # # # .
. . . . . S
```

The player is at `(0,0)`, the opaque wall run is `[0..4] x 1`, the open floor tile at `(5,1)`
is visible, and the opaque statue/fixture at `(5,2)` can become presented visible. Replacing the
diagonal window tile with a wall does not change the behavior, so this is not the translucent
aperture issue. It is an end-of-wall / around-corner visibility question.

There are two separate questions for tier-0 review:

1. Should the open floor tile `(5,1)` be visible at all?
2. If `(5,1)` is visible as a tiny sliver around the wall end, should that visibility be allowed to
   disclose or identify the opaque statue at `(5,2)`?

Under the current L1/L4 interval contract, `(5,1)` being volume-visible is expected. The tile square
has a positive-measure sliver around the end of the wall; the algorithm intentionally treats any
positive overlap with a surviving interval as visible. That is a permissive FOV policy, not a bug in
the interval bookkeeping. The statue, however, is more suspect: the disclosure layer currently checks
whether an origin-facing neighbor is cost-`0`, visible, and Transparent/Air. It does not prove that
the shared boundary between that neighbor and the candidate is itself exposed. A barely visible
corner sliver in `(5,1)` can therefore become enough support to disclose `(5,2)`.

"Shadow casting" here means **geometric FOV shadow casting**, not future frontend lighting. An opaque
tile removes angular intervals from sight propagation; that removed interval is the FOV shadow. A
renderer-side lighting system can later draw prettier darkness or cast visual shadows, but it cannot
repair engine truth after the engine has already marked a tile visible, presented, or explored. At
most, frontend lighting can make borderline visible tiles look less clear. The engine still needs to
decide which cells are visible and which surfaces may disclose.

Proposed review branches:

- **Branch A: keep permissive sliver visibility, tighten disclosure.** `(5,1)` remains volume-visible,
  but it cannot disclose `(5,2)` unless the discloser has boundary support toward the candidate. This
  means recording, or recomputing, which edge/corner of a cost-`0` open tile is actually exposed by
  the same interval geometry. This is the smallest semantic change and directly fixes the statue
  leak. It does not answer a stricter "no seeing around corners" rule.
- **Branch B: adopt a restrictive no-corner-peeking volume policy.** The volume layer stops treating
  every positive sliver as enough for open-tile visibility or propagation around solid corners. This
  could be implemented as a named corner-shadow policy, a minimum coverage / aperture threshold, or a
  center/footprint-supported visibility rule, but it must be one explicit geometry rule with oracle
  and benchmark updates. This branch would make `(5,1)` hidden in the repro and would naturally stop
  `(5,2)` as a follow-on. It is broader than a disclosure fix and may change other oblique wall-end
  views that the current oracle treats as valid.

Recommended tier-1 stance before Fable review: do not patch yet. Add the repro as a pending
characterization once the intended answer for `(5,1)` is chosen. If Fable accepts permissive sliver
visibility, implement Branch A. If Fable rejects corner peeking, write a new work order for Branch B
and explicitly supersede the current positive-measure rule for open floor volume.

Resolved: see the tier-0 verdict and work order immediately below (2026-07-04). Branch A adopted,
Branch B rejected.

### Tier-0 Verdict And Work Order: Boundary-Supported Disclosure (2026-07-04)

Review of the end-of-wall repro above. Both questions are answered; Branch A is adopted with a
specific locked mechanism; Branch B is rejected. Implement as written. No frontend change is part of
this order.

**Verdict on question 1 — `(5,1)` stays visible.** The sliver is geometric truth, not an artifact.
From the origin tile center there is a positive-measure bundle of clear sightlines (slopes strictly
between 1/11 and 1/9 in the repro) passing beyond the wall end at `(4,1)` into the near corner of
`(5,1)`'s square. Hiding it would make the engine strictly under-report reality. Decisive: the
oracle's hard rule — every sampled oracle-visible tile must be engine-visible at no worse cost —
certifies exactly this class of visibility. Branch B cannot be implemented without demoting the hard
rule to a soft report, which is breaking a system law to make behavior look nicer: the precise trap
this doc's work orders exist to prevent. If a future playtest shows open-floor slivers are
themselves confusing, that is a new work order superseding L1/L4 with a redesigned oracle, not a
patch.

**Verdict on question 2 — the statue disclosure is a false disclosure; adopt Branch A.** No
sightline from the origin reaches any face of `(5,2)`: the slopes covering its shared boundary with
`(5,1)` (3/11 to 1/3) lie entirely inside the wall-run shadow (1/9 to 1). The current neighbor
predicate is a proxy for "the open space touching this surface is clearly seen," and the proxy
breaks when the discloser is visible only through a sliver far from the shared boundary.

One structural fact shapes the mechanism, so it is recorded: **for lines through the origin center,
slope-span overlap is exactly equivalent to geometric line-square intersection** — any tile with a
center-visible interior point is already volume-visible before disclosure runs. Therefore every
disclosure-only candidate has no positive-measure-visible face at all, and disclosure is a courtesy
layer by construction: it presents a surface because the open space in front of it is seen, not
because the face itself is. The correct tightening tests that the open space *directly in front of
the shared face* is actually seen. Exact face-visibility tests would return "no" for room corners
too and kill the layer's founding use case.

**L17 — volume policy upheld.** Positive-measure sliver visibility for open tiles stays as locked in
L1/L4. Non-relitigatable without a superseding work order that also redesigns the oracle hard rule.

**L18 — boundary-supported disclosure.** Keep the current origin-facing neighbor scan in
`discloseSurfaces` as the cheap prefilter, unchanged. For each neighbor `N` that passes the
prefilter, disclosure of candidate `C` via `N` additionally requires a clear **support ray**: the
straight segment from the origin tile center to the support point `T(N, C)` must cross only
effectively Transparent/Air tiles (read through the effective-opacity cache), ending inside `N`.
Disclose on the first neighbor whose ray passes. `T(N, C)` is: for a cardinal neighbor, the midpoint
of the shared face between `N` and `C`, moved 1/4 tile from the face toward `N`'s center; for the
diagonal neighbor, the shared corner point moved 1/4 tile into `N` along both axes. Origin
exemption: when the prefilter-passing neighbor is the origin tile itself, disclose without a ray.
With tile centers at integers and the 1/4-tile inset, all relevant coordinates times 4 are integers:
compute grid-line crossings with exact integer arithmetic (the same cross-multiply discipline as
`FovSlope`), no floating point, no epsilons.

**L19 — support-ray semantics.** (a) Translucent (glass) blocks the support ray — disclosure remains
cost-0-only per L10. (b) If the segment passes exactly through a lattice corner, it is blocked
unless at least one of the two diagonally-straddling tiles it passes between is effectively
Transparent/Air — consistent with the corner-touching-walls invariant and with the permissive volume
philosophy (a measure-zero graze of a single wall corner does not blind an otherwise clear line).
Precisely (executor clarification, 2026-07-04): the straddling pair is the *pinching* pair, not the
enter/exit tiles along the ray. At a corner crossing the ray exits tile `A = (x, y)` and enters the
diagonally opposite tile `D = (x + sx, y + sy)` (direction signs `sx`, `sy` both nonzero); the
straddling pair is the other two tiles of the four meeting at that point: `(x + sx, y)` and
`(x, y + sy)`. Example: a slope-1 ray from origin center `(0,0)` crossing corner `(2.5, 2.5)` goes
from `(2,2)` into `(3,3)`; the tie-break pair is `(3,2)` and `(2,3)`. `A` and `D` are ordinary path
tiles checked by the normal walk; the straddling pair is tested only by this one-of-two rule and is
never treated as entered. Axis-aligned rays cannot hit a lattice corner (origin centers are
integers, support points lie on quarter coordinates), so the case arises only when both direction
components are nonzero.
(c) Both segment endpoints lie inside the clip window, so every tile the segment crosses is inside
the window and in bounds — do not add clamping or window-expansion logic for the ray.

Enumerated traps:

**T15 — do not implement boundary support with interval queries.** End-of-run per-octant intervals
include subtractions from occluders farther than the N–C boundary; testing boundary slopes against
them returns "unsupported" for room corners (whose only support is measure-zero and long since
subtracted) and silently kills the disclosure layer's founding case. Per-distance interval snapshots
would fix that but are unaffordable state. The support ray is the mandated mechanism.

**T16 — room corners hinge entirely on the diagonal-neighbor path.** For a room corner the cardinal
neighbors are walls; the only prefilter-passing neighbor is the diagonal floor, and its support
point is the shared corner moved diagonally into that floor. An implementation that only supports
cardinal faces, or skips the diagonal case as "no shared face," goes green on the statue and red on
`testRoomCornersAreDisclosedForPresentation`. That existing test is the over-tightening guard: if it
goes red, the rule is implemented too strictly (likely the diagonal target or the corner tie-break)
— fix the implementation, never loosen the test.

**T17 — disclosure is on the timed path; the bench rerun is mandatory this time** (unlike T14). Cast
rays only for candidates that pass the existing prefilter; no allocation in the ray walk; read
opacity via `GetOpacityByIndex`. The gate is unchanged: within 2x the 2026-07-03 pre-rewrite
snapshot. Record stress numbers in the History entry. Expected cost is a few dozen short rays per
compute; verify, don't assume.

**T18 — no existing green test may flip.** The new rule is strictly tighter than the current
predicate (a passing ray implies the prefilter's own conditions: the ray reaches `N`'s interior
through Transparent/Air only, so `N` is volume-visible at cost 0), so it can only remove
disclosures. Every disclosure the current suite asserts is legitimately face-fronted and must
survive. The window-clip differential in `fov-oracle.fsx` pins clipped/full disclosure parity and
must stay green unchanged. If any existing assertion flips, stop and bring it to review — do not
edit the test.

Mandated characterization tests (`fov-tests.fsx`):

1. The end-of-wall repro verbatim: origin `(0,0)`, opaque wall run `[0..4] x {1}`, open floor at
   `(5,1)`, opaque **fixture** (statue) at `(5,2)` — a fixture, not a wall tile, so the
   effective-opacity path is covered. Assert `(5,1)` is volume-visible (pins L17); assert `(5,2)` is
   not disclosed, not presented-visible, not explored, and reports `presentedDepth = -1`.
2. Corner tie-break: a map where the only support ray for a candidate crosses a lattice corner
   flanked by two opaque tiles — disclosure must not fire; flip one flanking tile to floor —
   disclosure must fire.
3. `testRoomCornersAreDisclosedForPresentation` stays green (the T16 guard — no new test needed,
   but its result must be reported with the run).

Verification before timing: `dotnet build`, full `fov-tests.fsx`, and `fov-oracle.fsx` (hard rule
and window-clip differential) all green **before** any benchmark run; then the bench, with numbers
recorded against the snapshot in the History entry.

### Amendment (2026-07-04, resolved by the verdict below): direct surface presentation can lose to a glass sliver

Post-boundary-support playtest found one remaining near-window presentation artifact. In screenclip
local coordinates, with top left `(0,0)`, player at `(3,2)`, a translucent window at `(2,2)`, and an
opaque wall tile at `(2,1)`, the wall tile above the window can render blurred. A minimal shape for
the engine-side diagnosis is:

```text
.....
..##.
..G@.
.....
```

With translucency budget `1`, the current engine reports the target wall `(2,1)` as
`visible = true`, `disclosed = false`, `translucencyDepth = 1`, `presentedDepth = 1`, and
`explored = false`. If the wall at `(3,1)` is changed to floor, the same target reports depth `0`.
That points to a presentation precedence issue, not a frontend blur bug: the interval layer finds a
positive-measure sliver to `(2,1)` through the window, so the tile is volume-visible at cost `1`.
Because disclosure currently skips volume-visible cells, the direct surface/corner presentation
layer never gets a chance to say "this nearby wall face is also directly presented."

Proposed fix: keep volume FOV exactly as it is, but let the surface-presentation pass evaluate
non-open candidates even when they are already volume-visible. If the candidate has the same direct
surface support that disclosure already requires, stamp the presentation surface and let
`presentedDepth` choose the clearer direct surface depth (`0`) over the through-glass volume depth
(`>= 1`). `RectFov.translucencyDepth` should remain pure volume cost, so diagnostics and future
lighting can still see that a glass sliver exists.

This should be treated as a presentation override, not as new propagation. It must not add angular
intervals, must not make hidden cells volume-visible, and must not make the old far-bookshelf or
end-of-wall statue leaks return. The likely implementation choices are either to allow
`disclosedStamp` to mean "surface-presented" for both hidden and visible non-open cells, or to add a
separate `surfaceStamp` if the naming and exploration semantics become muddy.

Review questions for Fable before coding:

1. Should the existing origin-adjacent disclosure exemption apply when the candidate is already
   volume-visible through glass, or should diagonal origin cases still check a pinching pair when
   the two side tiles are wall/glass?
2. Should a volume-visible cost-`1` wall that receives direct surface presentation become explored
   immediately, matching current disclosed-only surfaces, or should exploration continue to follow
   volume cost only in this mixed case?
3. Is `presentedDepth = min(volumeDepth, surfaceDepth)` the right public contract, with
   `translucencyDepth` left unchanged as the volume-only query?

Mandated characterization tests if approved:

1. The near-window repro above: assert `(2,1)` is volume-visible at depth `1`, surface-presented or
   disclosed for presentation, `presentedDepth = 0`, and does not alter visibility behind the wall.
2. The open-neighbor variant with `(3,1)` floor stays depth `0`, proving the new rule does not
   regress ordinary direct wall sight.
3. The L16 bookshelf/pillar through-glass regression stays depth `1` when no direct surface support
   exists.
4. The end-of-wall statue regression stays not presented-visible; the new rule must not weaken
   boundary-supported disclosure.

### Tier-0 Verdict: Direct Surface Presentation Override (2026-07-04)

The amendment above is approved in the shape proposed, with the three review questions answered and
locked below. This is a presentation-precedence fix, not new propagation. The decisive argument is
**monotonicity**: under budget `0` the repro wall is hidden from volume FOV and the origin exemption
discloses it at presented depth `0`; under budget `1` the glass sliver makes it volume-visible at
cost `1`, today's pass skips it as "already visible," and it blurs. The engine gaining information
must never degrade presentation. The same inversion appears from the other side: replacing the
window with solid wall makes the adjacent corner render *clearer*. Both absurdities vanish once
surface support is also evaluated for volume-visible non-open cells.

**Q1 — the origin exemption applies unchanged, including to volume-visible candidates; no
pinching-pair check is added for origin-adjacent cases.** The exemption is a proximity courtesy —
"you are standing against this surface" — and is deliberately non-geometric. It already presents
fully sealed diagonal corners whose pinching pair is two opaque walls (the adjacent-room-corner
scenario in `testRoomCornersAreDisclosedForPresentation`); a wall+glass pinch is strictly weaker
sealing than that precedent. This answer is load-bearing for the repro itself: `(2,1)`'s only
prefilter-passing neighbor is the origin (`(3,1)` is wall, `(2,2)` is glass), so a pinch check on
origin-diagonal cases would leave the repro blurred and simultaneously regress the adjacent-corner
test.

**Q2 — surface-presented tiles are marked explored immediately, exactly like disclosed-only
tiles.** Locked invariant: presented depth `0` implies explored at that tick (cost-`0` volume
already explores; disclosure already explores; the new override explores). Anything else creates
memory asymmetry — crystal clear now, forgotten later. This does not explore general through-glass
tiles, only surface-supported ones, so the pinned "tiles behind glass carry depth without being
explored" behavior is untouched.

**Q3 — yes, with a phrasing correction: do not introduce a surface-depth quantity.** Surface
presentation is identically depth `0`, so the public contract is a two-case rule, not a `min` over
two depth fields: `presentedDepth` = `0` if surface-presented, else the volume translucency cost if
volume-visible, else `-1`. `translucencyDepth` stays pure volume cost (L16 discipline).
`isPresentedVisible`'s truth value is unchanged — volume-visible tiles were already presented.

Locked decisions:

**L20 — the surface pass also evaluates volume-visible non-open cells with volume cost >= 1**,
under the exact same support rule as disclosure (prefilter, support ray, origin exemption — all
unchanged from L18/L19). Volume-visible cells at cost `0` are skipped: their presentation is
already direct and they are already explored. Hidden-cell disclosure is unchanged. The volume layer
(intervals, `isVisible`, cost array) is untouched.

**L21 — presented depth contract**: surface-presented => `0`; else volume cost if volume-visible;
else `-1`. No third case, no separate surface-depth field.

**L22 — exploration**: surface-presented => explored, same tick, same as disclosed-only surfaces.

**L23 — one stamp array, backward-compatible queries.** Reuse the existing stamp array (rename it
internally to "surface-presented" if that helps); do not add a second array. `isDisclosed` keeps
its current observable meaning — surface-presented AND NOT volume-visible — implemented at query
level (`stamp = tick && visibleStamp <> tick`), so every existing `assertDisclosed` /
`assertNotDisclosed` stays literally green. `isPresentedVisible` remains volume-or-stamp.

Enumerated traps:

**T19 — candidate gating is the perf fix; get it wrong and the pass rays every wall in view.** The
extension may only add candidates that are non-open AND volume-visible AND cost >= 1. Such cells
exist only near translucent tiles, so the added ray work is sparse. The bench rerun is mandatory
(the pass is on the timed path); the gate is unchanged at 2x the 2026-07-03 snapshot; current
stress `3.03-3.33 ms` leaves headroom. Record numbers in the History entry.

**T20 — depth-assertion flips must be classified, never edited.** The override can legitimately
flip an existing presented-depth assertion only for a candidate with a passing support rule
(cost-`0` fronting neighbor or origin adjacency). Analysis says zero existing tests are in that
class — their through-glass tiles have cost >= 1 neighbors only. Run the full suite first; any flip
must be shown to be this intended class and listed in the History entry; anything else, stop and
bring to review.

**T21 — do not fold the override into `translucencyDepth`.** The tempting "simplification" —
returning `0` from `translucencyDepth` for surface-presented tiles — recreates the L16 legacy
override in new clothes: a presentation concern leaking back into the volume-truth query. The
override lives in `presentedDepth` only. Likewise do not extend the dense oracle to cover this
(T14 discipline); the window-clip differential inherits the extended stamp automatically and must
pass unchanged.

Mandated tests: the four listed in the amendment are approved, with additions to test 1 — on the
repro target also assert `translucencyDepth = 1` (pins T21), `isDisclosed = false` (pins L23), and
explored (pins L22). Recommended fifth, cheap: the same repro at budget `0` — target hidden from
volume, disclosed via the origin exemption, presented depth `0` — pinning the monotonicity argument
at both budgets.

Verification before timing: `dotnet build`, full `fov-tests.fsx`, and `fov-oracle.fsx` (hard rule
and window-clip differential) green **before** the mandatory bench rerun; numbers recorded against
the snapshot in the History entry.

## Surface Presentation Implementation

The 2026-07-04 implementation adds the approved surface-presentation layer from the work order above. `VisibilityState` carries one surface stamp beside `visibleStamp` and the translucency cost array. `RectFov.isVisible` remains volume-only. `RectFov.isDisclosed` reports only surface-presented cells that are not volume-visible. `RectFov.isPresentedVisible` and `RectFov.presentedDepth` expose the renderer-facing presentation layer.

The surface pass runs once after all octants complete. It scans only the clipped window and can stamp only non-open cells. Hidden cells use the original disclosure rule. Volume-visible cells are considered only when their volume translucency cost is `>= 1`, so direct cost-`0` cells do not waste support-ray work. In both cases, the origin-facing neighbor set must contain a cost-`0` volume-visible Transparent or Air cell. Since L18, a non-origin discloser must also have boundary support: an exact integer support ray from the origin tile center to the shared face/corner inset inside that discloser must cross only effectively Transparent or Air path tiles. Translucent glass blocks this support ray. If the support ray crosses exactly through a lattice corner, the pinching side pair blocks only when both side tiles are not Transparent/Air.

Surface-presented tiles are marked explored and presented with depth `0`. They do not alter interval state, volume cost, or `isVisible`. This means `RectFov.translucencyDepth` can still report that a nearby wall was volume-visible through a window at cost `1`, while `RectFov.presentedDepth` reports `0` because the wall face also has direct surface support.

The frontend handoff is now written at the top of `FOV.fs`: render lit state from the presented query, not raw volume visibility, and dim presented depth `>= 1` so through-glass visibility does not look like direct sight. Stage B coverage remains deferred until playtest proves depth dimming is insufficient.

The remaining playtest issue after this handoff — far-corner pillars and the bookshelf still
perfectly clear through windows while floors behind the same glass dim — is diagnosed and its fix is
specified in the work-order amendment above (L16). The proposed anchor-tile diagnostic resolved to
the engine branch: those anchors report presented depth `0`, because `translucencyDepth` carries a
legacy explored-override that returns `0` for explored effectively-Opaque/Translucent tiles even
when they are currently seen only through glass at cost `>= 1`. The frontend fog path was already
correct. The sprite-footprint hypothesis is not the cause; at most it can produce minor mask-boundary
seams, and it warrants no action unless artifacts remain after L16 lands.

## Limitations

`fov-tests.fsx` now pins down the core FOV contract: translucent tiles are visible before they spend budget, tiles behind glass carry translucency depth without being explored, previously explored opaque fixtures seen through glass keep their through-glass depth, opaque faces block, zero-budget glass should not leak into side pockets, shallow-angle glass faces next to visible floor should still be shown, opaque fixtures behind a wall next to a doorway should stay hidden, direct opaque fixtures in open sight should be visible and blocking, corner-touching walls should not leak sight, room corners should be disclosed, open diagonal floor around a solid corner should stay dark, glass should not disclose the wall behind it, walls should not disclose walls behind them, an end-of-wall floor sliver should not disclose the fixture behind it, near-window wall surfaces should present clearly without changing volume depth, support-ray corner pinches should block only when both pinching tiles are opaque, translucent seam tiles should not double-spend budget, and large windows should use the same visibility rules as small windows.

`fov-oracle.fsx` is a dense sampled witness oracle, not a proof of exact equivalence. Its hard rule is authoritative: every sampled oracle-visible tile must be visible in the engine at no worse cost. Its soft reports are engine-visible tiles missed by finite sampling. The current soft class is dominated by narrow oblique wall/glass slivers that are expected from positive-measure square spans and were not treated as merge blockers after the hard oracle passed.

The end-of-wall corner-peeking question is resolved by tier-0 verdict and implementation
(2026-07-04): permissive sliver visibility of open tiles is upheld as geometric truth (L17), and
disclosure is tightened so a sliver cannot vouch for a surface whose shared boundary it does not
front (L18/L19, boundary-supported disclosure). The recorded statue leak is covered by
`testEndOfWallSliverDoesNotDiscloseFixtureBehindIt`. The residual permissive behavior -- seeing a
genuine corner sliver of open floor past a wall end -- is intended and oracle-certified.

The near-window wall blur is resolved by implementation (2026-07-04, direct surface presentation
override, L20-L23): the surface pass also evaluates volume-visible non-open cells at cost >= 1
under the same support rule, and `presentedDepth` prefers the direct surface (`0`) over the
through-glass volume cost. `translucencyDepth` remains pure volume cost.

There is a remaining aesthetic tradeoff around translucent barriers such as portcullises. A local
cluster can be geometrically correct but visually discordant: the portcullis surface itself is
direct and clear, floor beyond it can be currently visible through translucency but unexplored, and
a nearby wall beyond the same barrier can be explored and therefore brighter while still blurred by
`presentedDepth >= 1`. The engine data is intentionally preserving those distinctions. If this reads
poorly in authored maps, prefer a named presentation/editor solution over changing core FOV rules:
for example, a local editor override or tile presentation hint that tunes brightness/blur for that
specific composition. Do not make all boundary walls ignore translucency; genuinely far-side walls
seen through windows or portcullises should still read as through-translucency.

`VisibilityState` is fixed-size. If the map dimensions change, create a matching new state instead of reusing the old one.

`VisibilityState` is not automatically fresh after model creation or mutation. After `GameUpdate.create*`, it starts empty until the first explicit `GameUpdate.recomputeVisibility`. After movement, interaction, or visibility-setting changes, callers should treat it as stale until they explicitly recompute. This keeps the engine from doing map-sized visibility work at gameplay-mutation cadence when the presentation layer owns the camera window.

Current visibility is not serialized. Only exploration lives on `TileMap` and participates in map persistence.

## Source Map

- `FOV.fs` - `VisibilityState`, exact slope intervals, surface presentation, `RectFov.compute`, volume and presented query helpers, interval occlusion, and explored marking.
- `Maps.fs` - runtime `TileMap`, effective opacity cache, `Explored`, `MarkExplored`, `IsExplored`, and `ClearExplored`.
- `Types.fs` - `TileOpacity`.
- `GameState.fs` - `GameUpdate.recomputeVisibility`.
- `fov-tests.fsx` - characterization tests for translucency, exploration, blocking, disclosure, presented visibility, and the known over-permissive/over-strict artifact risks.
- `fov-oracle.fsx` - independent dense sampled oracle for hard witness checks and soft over-visibility triage, plus the window-clip differential check that pins the rectangular clipping and disclosure paths.
- `fov-bench.fsx` - current benchmark script and older inline timing notes.
- `archive/fov-gpt5.2-alt.txt` - archived alternate FOV implementation.
- `archive/fov-opus-alt.txt` - archived alternate FOV implementation.
- `archive/fov-simpler-alt.txt` - archived simpler FOV implementation.

## History

### 2026-07-08 (visibility cache ownership)

- Made `GameModel.VisibilityState` a derived cache refreshed only by explicit `GameUpdate.recomputeVisibility`.
- Recorded the lifecycle law: engine mutators report `Changes.VisibilityInputChanged` but do not eagerly recompute visibility; creation starts with an empty cache, and setters only update the fields used by the next compute.
- This removes hidden map-sized FOV work from movement and interaction paths while preserving engine ownership of visibility state for future simulation consumers.

### 2026-07-04 (portcullis presentation aesthetic tradeoff)

- Recorded a visual tradeoff around translucent barriers: a clear portcullis, dim
  unexplored floor beyond it, and brighter explored wall beyond it can be geometrically correct but
  visually discordant in top-down tile art.
- Classified this as presentation/aesthetic policy rather than an FOV correctness bug. The suggested
  future escape hatch is a named local editor override or tile presentation hint, not a broad rule
  making boundary walls ignore translucency.

### 2026-07-04 (documentation clarification: nonstandard FOV)

- Added a dedicated explanation of why this system is not ordinary binary tile FOV: it separates
  volume visibility, surface presentation, translucency depth, and persistent exploration.
- Tightened the live explanation so it names the current four answers directly and matches the
  final surface-presentation override behavior in `FOV.fs`.
- No engine behavior changed.

### 2026-07-04 (direct surface presentation override implementation)

- Implemented L20-L23 in `FOV.fs`: the surface pass now also evaluates non-open, volume-visible
  candidates with translucency cost `>= 1`; cost-`0` volume cells are skipped. The same prefilter,
  support-ray rule, and origin exemption from L18/L19 are reused unchanged.
- Reused the existing stamp array as the surface-presentation stamp. `RectFov.isDisclosed` now means
  surface-presented and not volume-visible, while `RectFov.isPresentedVisible` remains
  volume-visible or surface-presented.
- Updated `RectFov.presentedDepth`: surface-presented tiles report depth `0`; otherwise
  volume-visible tiles report their volume translucency cost; otherwise the result is `-1`.
  `RectFov.translucencyDepth` remains pure volume cost.
- Added the near-window wall repro at budget `1`, the monotonic budget-`0` variant, and the
  open-neighbor direct-wall guard. The existing through-glass bookshelf and end-of-wall statue
  regressions stayed green.
- Verified with `dotnet build -v:minimal`, `dotnet fsi .\fov-tests.fsx`, `dotnet fsi
  .\fov-oracle.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\map-tests.fsx`, and `dotnet fsi
  .\entity-registry-test.fsx`.
- Benchmark after the override: 1080p-ish `0.2553 ms`, 1440p-ish `0.5009 ms`, 4k-ish `1.1097 ms`,
  stress `3.7347 ms`. This remains within the `2x` gate against the 2026-07-03 snapshot.

### 2026-07-04 (tier-0 verdict: direct surface presentation override)

- Approved the near-window amendment as a presentation-precedence fix. Decisive argument:
  monotonicity — at budget `0` the repro wall is disclosed at presented depth `0` via the origin
  exemption, while at budget `1` the glass sliver makes it volume-visible at cost `1` and blurs it;
  the engine gaining information must never degrade presentation. Equivalent absurdity: swapping
  the window for solid wall renders the adjacent corner clearer.
- Answered the review questions: origin exemption applies unchanged to volume-visible candidates
  (load-bearing for the repro, whose only prefilter-passing neighbor is the origin; a pinch check
  would also regress the adjacent-corner test); surface-presented tiles explore immediately
  (invariant: presented depth `0` => explored); `presentedDepth` is a two-case rule with no
  separate surface-depth quantity.
- Locked L20-L23: candidates gated to non-open, volume-visible, cost >= 1 (cost-`0` cells skipped);
  the depth contract; exploration on surface presentation; a single stamp array with `isDisclosed`
  meaning surface-presented AND NOT volume-visible at query level so existing assertions stay
  green.
- Issued T19-T21: candidate gating is the perf fix and the bench rerun is mandatory; any existing
  depth-assertion flip must be classified as the intended surface-supported class, never edited;
  do not fold the override into `translucencyDepth` — that recreates the deleted L16 legacy
  override.
- Approved the amendment's four mandated tests, added `translucencyDepth = 1`,
  `isDisclosed = false`, and explored assertions to the repro test, and recommended a budget-`0`
  variant pinning monotonicity at both budgets.

### 2026-07-04 (open review: near-window wall blur)

- Recorded the screenclip-local repro: player `(3,2)`, translucent window `(2,2)`, target opaque wall
  `(2,1)`, and opaque side wall `(3,1)`. Minimal rows: `"....."`, `"..##."`, `"..G@."`, `"....."`.
- Confirmed the engine reports the target as volume-visible through glass:
  `visible = true`, `disclosed = false`, `translucencyDepth = 1`, `presentedDepth = 1`, and
  `explored = false`. Opening `(3,1)` makes the same target depth `0`.
- Diagnosed this as a presentation precedence gap: a through-glass positive-measure sliver can win
  before surface disclosure gets a chance to present the nearby wall face directly.
- Proposed a tier-0-reviewed fix: allow the surface-presentation pass to stamp already-visible
  non-open candidates when the existing disclosure support rule passes, then have `presentedDepth`
  prefer the direct surface depth while keeping `translucencyDepth` volume-only.

### 2026-07-04 (boundary-supported disclosure implementation)

- Implemented L18/L19 in `FOV.fs`: disclosure still uses the origin-facing cost-`0`
  Transparent/Air neighbor prefilter, but non-origin disclosers now need an exact integer support ray
  from the origin center to the shared face/corner inset inside the discloser. Translucent tiles block
  the support ray. Lattice-corner crossings use the pinching side pair, not the diagonal enter/exit
  tiles.
- Added `testEndOfWallSliverDoesNotDiscloseFixtureBehindIt`, which pins the recorded repro: `(5,1)`
  remains volume-visible, while the fixture at `(5,2)` is not disclosed, not presented, not explored,
  and has presented depth `-1`.
- Added `testDisclosureSupportRayCornerTieBreak`, covering the pinching-pair rule: a sealed corner
  blocks disclosure, while opening one pinching tile lets the diagonal support ray disclose the
  corner. `testRoomCornersAreDisclosedForPresentation` stayed green.
- Implemented the tier-0 "free win" optimization in `minOverlappingCostFrom`: stop scanning when cost
  `0` is found. This was added after an initial benchmark sample put stress just over the nominal
  gate (`4.1385 ms`); post-optimization samples were back inside the gate.
- Verified with `dotnet build -v:minimal`, `dotnet fsi .\fov-tests.fsx`, and `dotnet fsi
  .\fov-oracle.fsx`. Oracle hard checks and the disclosure window-clip differential passed; the
  expected soft finite-sampling reports remain.
- Post-optimization benchmark samples: 1080p-ish `0.2973-0.3670 ms`, 1440p-ish `0.5264-0.5461 ms`,
  4k-ish `1.0386-1.1070 ms`, stress `3.0288-3.3343 ms`.

### 2026-07-04 (tier-0 verdict: boundary-supported disclosure)

- Ruled on the end-of-wall open review: Branch A adopted, Branch B rejected. `(5,1)`'s sliver is
  geometric truth (clear sightlines at slopes strictly between 1/11 and 1/9 past the wall end) and
  is protected by the oracle hard rule; the statue disclosure is geometrically false (its shared
  boundary spans slopes 3/11 to 1/3, entirely inside the wall shadow 1/9 to 1).
- Recorded the structural fact that decided the mechanism: center-line slope-span overlap equals
  exact line-square intersection, so disclosure-only candidates never have a positive-measure
  visible face — disclosure is a courtesy layer, and the tightening must test the open space in
  front of the shared face (support ray), not face visibility or end-of-run intervals, both of which
  would kill room-corner disclosure.
- Issued work order: L17 (volume policy upheld), L18 (support ray from origin center to the shared
  face midpoint inset 1/4 tile into the discloser; diagonal case insets the shared corner; origin
  exemption; exact integer arithmetic), L19 (glass blocks the ray; lattice-corner crossings blocked
  only when both straddling tiles are opaque; no window clamping needed), T15–T18, three mandated
  characterization outcomes, and a mandatory bench rerun since disclosure is on the timed path.

### 2026-07-04 (open review: end-of-wall corner peeking)

- Recorded the non-glass repro with player `(0,0)`, wall run `[0..4] x 1`, visible floor `(5,1)`,
  and statue/fixture `(5,2)`.
- Split the issue into two review questions: whether the open floor sliver should be visible at all,
  and whether such a sliver should be enough to disclose the statue behind it.
- Clarified that "shadow casting" in this context means geometric FOV interval shadowing, not future
  frontend lighting shadows.
- Proposed two review branches for Fable: boundary-supported disclosure if permissive sliver
  visibility stays, or an explicit no-corner-peeking volume policy if `(5,1)` should be hidden too.

### 2026-07-04 (L16 through-glass depth fix)

- Deleted the legacy explored-opaque/translucent override from `RectFov.translucencyDepth`. Volume-visible tiles now report the current volume translucency cost regardless of exploration history or effective opacity; disclosed-only surfaces still present at depth `0` through `RectFov.presentedDepth`.
- Added `testExploredOpaqueFixtureThroughGlassKeepsDepth`, a two-compute regression that first explores a room from inside, then views the same explored floor and opaque bookshelf fixture through glass and asserts both report volume/presented depth `1` while the directly seen glass pane remains depth `0`.
- Verified with `dotnet build -v:minimal`, `dotnet fsi .\fov-tests.fsx`, `dotnet fsi .\fov-oracle.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\map-tests.fsx`, and `dotnet fsi .\entity-registry-test.fsx`. No benchmark rerun was needed because the change is query-only and not on the timed compute path.

### 2026-07-04 (tier-0 diagnosis: through-glass over-clarity root cause)

- Resolved the open diagnostic fork from the post-disclosure observation: the object anchor tiles
  report presented depth `0`, so the fault is engine-side, not frontend sprite handling.
- Root cause: the legacy explored-override in `RectFov.translucencyDepth` (inherited verbatim from
  the pre-rewrite FOV) returns depth `0` for explored effectively-Opaque/Translucent tiles seen
  through glass, so explored walls, pillars, and bookshelves present crystal clear while floors dim.
- Verified with a two-compute repro: the far wall through glass reports volume cost `1` but
  presented depth `0` when previously explored, and depth `1` when unexplored; floors report `1` in
  both cases.
- Established that no existing test pins the override — single-compute tests never produce an
  explored tile at volume cost `>= 1` — which is why it survived the interval rewrite and the
  disclosure review gate unnoticed.
- Issued work-order amendment L16 (delete the override; presented depth is pure volume depth) with
  T13 (mandated two-compute regression test, including an opaque fixture) and T14 (query-only
  change; volume layer, oracle, and benchmark gate untouched). No frontend change required.

### 2026-07-04 (post-disclosure frontend observation)

- Recorded the remaining over-clarity issue after surface disclosure: sprites such as far corner pillars and the bookshelf can still appear clear through windows even when the tile fog/depth mask is using presented visibility.
- Current hypothesis: `GameSpriteOverlay.cs` applies `RectFov.isPresentedVisible` and `RectFov.presentedDepth` to the fog texture, but entity sprite rendering still uses a separate visible/alpha path that ignores translucency depth. Sprite footprint mismatch may also let tall or wide sprites extend outside the depth mask for their anchor tile.
- Next diagnostic: render or inspect presented depth at the object anchor tiles. Depth `1+` points to frontend sprite modulation, mask-footprint, draw-order, or material handling; depth `0` points to needing a richer engine presentation category for disclosed-through-translucency surfaces.

### 2026-07-04 (surface disclosure implementation)

- Implemented the approved surface-disclosure layer as a separate `VisibilityState` stamp. `RectFov.isVisible` remains volume-only; `RectFov.isDisclosed`, `RectFov.isPresentedVisible`, and `RectFov.presentedDepth` expose presentation state.
- Added frontend comments at the top of `FOV.fs`: render lit state from presented visibility and dim by presented depth so cost `>= 1` through-glass sight is not drawn crystal clear.
- Added disclosure tests for room corners, adjacent corner disclosure, around-corner open floor staying dark, the doorway-adjacent bookshelf remaining hidden/undisclosed/unexplored, glass not disclosing a wall behind it, and wall-behind-wall not disclosing.
- Extended the oracle window-clip differential check to compare disclosure stamps while keeping the dense sampled oracle volume-only.
- Verification passed with `dotnet build -v:minimal`, `dotnet fsi .\fov-tests.fsx`, `dotnet fsi .\fov-oracle.fsx`, `dotnet fsi .\fov-bench.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\map-tests.fsx`, and `dotnet fsi .\entity-registry-test.fsx`.
- Benchmark range with disclosure included: 1080p-ish `0.3246-0.3775 ms`, 1440p-ish `0.5193-0.6573 ms`, 4k-ish `1.1284-1.2260 ms`, stress `3.3672-3.9271 ms`. The stress line remains close to, but inside, the `2x` gate.

### 2026-07-04 (tier-0 review of playtest issues)

- Reproduced all three reported artifact classes in isolation and pinned root causes: permanently
  black corners are exact L1 measure-zero geometry (double-bracketed corner tiles), not an engine
  bug; wall runs are unaffected; window over-clarity is the frontend ignoring the already-exposed
  translucency depth, not missing engine data.
- Issued the surface-disclosure work order (L9–L15, T7–T12): origin-facing-neighbor disclosure as a
  separate presentation stamp, disclosure-implies-explored, `isVisible` kept volume-pure for the
  oracle, window-clip differential check extended to disclosure stamps.
- Cut the proposed six layers to scope: local visibility baseline rejected (around-corner leak),
  aperture transmission deferred (content feature + oracle rework), coverage staged behind a
  depth-dimming playtest, exploration amended minimally.

### 2026-07-04

- Recorded playtest-visible issues after the interval rewrite: some corners remain permanently black, some explored corners stay permanently dark, near-player top-left/top-right tiles can be explored but not currently visible, and window views can make neighboring and distant tiles appear too clear.
- Added the next design direction: keep interval propagation, but split presentation into volume visibility, local visibility, non-propagating surface disclosure, translucent aperture transmission, visibility strength or coverage, and explicit exploration semantics.
- Clarified that the proposed fix should not restore the old disabled repair paths; any new layer must be named, constrained, and tested independently.

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
