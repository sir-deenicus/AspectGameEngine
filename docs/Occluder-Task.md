# Chunk Occluder

This document explains how the current chunk occluder works. The implementation lives in `ChunkOcclusionManager.fs`.

The short version: the manager watches a tile-sized opacity grid, groups solid cells into larger rectangles, and lets the caller ask for just the occluder rectangles that overlap the current view. It is designed to make map visibility or shadow geometry cheap to update when only a few tiles change.

## Explanation

The occluder system is built around a simple idea: most opaque map cells do not need to become separate occluder objects. A wall that is ten tiles long can usually be represented as one rectangle instead of ten little blocks. If the map is split into chunks, only the chunk containing a changed tile needs to be rebuilt.

`ChunkOcclusionManager` owns its own internal view of which cells are opaque. It does not read directly from `TileMap` every frame. Instead, callers feed it updates through methods such as `SetOpaque`, `SetFromTileAndLayer`, and `SetDoorCellState`. When a cell changes, the manager marks that cell's chunk dirty. Later, `RebuildDirty()` rebuilds only those dirty chunks.

The output shape is `RectI`:

```fsharp
type RectI = { X:int; Y:int; W:int; H:int }
```

These rectangles are in tile coordinates. `X` and `Y` are the top-left tile of the rectangle, and `W` and `H` are tile counts. A rectangle `{ X = 4; Y = 3; W = 2; H = 5 }` covers tiles from x `4` through `5`, and y `3` through `7`.

## Data It Owns

The manager keeps four main pieces of state:

- `opaque`: a flat boolean grid saying whether each map cell currently blocks occlusion.
- `isDoorCell`: a flat boolean grid saying whether each cell uses the current door-specific dynamic path.
- `rectsPerChunk`: merged opaque rectangles for ordinary cells in each chunk.
- `doorRectsPerChunk`: dedicated rectangles for currently closed door cells.

There is also a `dirtyChunks` set. A chunk enters that set when a relevant cell in the chunk changes opacity or door state. Rebuilds clear the set after refreshing the affected chunks.

The manager uses the same flat indexing convention as the rest of the map code:

```text
index = y * width + x
```

## Feeding Opacity

There are three public update paths.

`SetOpaque(x, y, value)` is the direct path. It changes the manager's internal opaque value for a cell and marks the chunk dirty if the value actually changed.

`SetFromTileAndLayer(x, y, baseTileOpacity, layerCell)` is the convenience path for ordinary cells. It asks `LayerQueries.EffectiveTileOpacity` whether the base tile plus actor or fixture should be opaque. Items and decals do not affect opacity. If the cell is already known as a door cell, this method leaves it alone because doors have their own update path.

`SetDoorCellState(x, y, opacity)` is the door path. It marks the cell as a door and sets its current opaque value from the door opacity. Door cells are deliberately excluded from the normal merged wall rectangles.

The current API names doors because doors are the dynamic occluder source implemented today. The broader rule is not door-specific: any occluder source that can change independently and often should avoid being fused into long-lived merged geometry unless the system has a cheap way to split it back out.

## Rebuilding Chunks

`RebuildDirty()` loops over the dirty chunk set. For each dirty chunk, it clears that chunk's old output rectangles and rebuilds them from the internal grids.

The rebuild has two passes.

First, it scans opaque non-door cells and greedily merges them into rectangles. Starting from the first unvisited solid cell, it grows a rectangle as wide as possible, then grows it downward while every cell in the next row is still solid and unvisited. Those cells are marked visited and one rectangle is emitted.

Second, it scans the same chunk for door cells. If a door cell is opaque, it emits a dedicated `1x1` rectangle. If the door is transparent, it emits nothing for that door.

The manager merges only inside one internal chunk at a time. Here, "boundary" means the invisible chunk grid line every `chunkSize` tiles, not a gameplay or physics boundary. If one continuous wall crosses from chunk A into chunk B, it becomes at least one rectangle in chunk A and one rectangle in chunk B. That is a deliberate bounded-cost tradeoff in the current implementation: chunk rebuilds stay local, and the caller can still iterate the resulting rectangles as one combined stream.

## Querying Rectangles

There are two ways to read output.

`GetChunkRects(cx, cy)` returns the merged non-door rectangles for a chunk.

`GetChunkDoorRects(cx, cy)` returns the closed-door rectangles for a chunk.

Most callers should prefer `ForEachRectInView(viewX0, viewY0, viewX1, viewY1, action)`. It converts the view bounds into a chunk range, visits the wall rectangles and door rectangles in those chunks, and calls `action` only for rectangles that overlap the requested view.

The view bounds are best treated as exclusive on the far edge:

```text
view covers x >= viewX0 and x < viewX1
view covers y >= viewY0 and y < viewY1
```

That matches the overlap test in the implementation.

## Focused Tests

Focused coverage lives in `occluder-tests.fsx`. The script characterizes the current rectangle-only contract rather than trying to drive renderer behavior.

The covered cases are empty managers, single cells and removal, horizontal and vertical run merging, rectangular block merging, separated blocks, chunk-boundary splitting, dedicated closed-door rectangles, door open/closed toggling, `SetFromTileAndLayer` effective opacity from base tiles, fixtures, and actors, `ClearAll`, and `ForEachRectInView` far-edge-exclusive filtering.

## How It Fits The Map

The occluder manager is not the authoritative map. `TileMap` owns tiles, layer cells, and the effective opacity cache. The occluder manager owns a derived occluder grid and derived occluder geometry.

In the common path, the caller should update the map first, then update the occluder manager for any cells whose effective opacity changed. Once those updates are batched, call `RebuildDirty()` and use `ForEachRectInView` to build or refresh the renderer-side occlusion geometry.

The key ownership rule is that the map changes first, then the occluder manager is told what changed. Doors currently have a named helper because their open or closed visual state can change without the base tile ceasing to be a door. Other mutable blockers can still be represented by feeding changed cells through `SetOpaque` or `SetFromTileAndLayer`.

## Mutable Map Changes

The environment is expected to be mutable. Doors, construction, destructive spells, terrain edits, fixture placement, temporary force barriers, summoned obstacles, ice walls, smoke, fog, glass, and other gameplay systems can change which cells block sight or shadow.

The occluder manager supports that style by being incrementally fed the changed cells. A door toggle currently calls `SetDoorCellState`. A destructive spell that removes blocking tiles should update the map, then call `SetOpaque` or `SetFromTileAndLayer` for each changed cell. A constructive spell that creates blocking terrain, ice, force, smoke, or other occluding geometry should do the same in the other direction.

If a spell or edit touches cells in several chunks, each affected chunk is marked dirty as the changed cells are fed in. `RebuildDirty()` then rebuilds all affected chunks, while untouched chunks keep their old rectangle output.

## Tradeoffs

The current output contract is rectangle-only. That is fine for the current engine direction: it keeps the manager simple, easy to test, and easy for a renderer to consume. The archive file `chunkoccluder-inset.txt` sketches an alternate design with wall edges and door edges, but that is not the live implementation or the current plan.

Separating independently mutable occluders from broader merged geometry is a deliberate tradeoff. Doors are the current concrete example, but the same idea applies to any cell or region whose opacity can change without the surrounding geometry changing. If that source is merged into a larger rectangle, changing it later may require splitting or rebuilding the larger shape. Keeping it as separate output keeps updates local and predictable.

Rectangles not merging across internal chunk grid lines is also a deliberate tradeoff. It creates more rectangles than a whole-map greedy merge, but it prevents one edit from invalidating geometry for a huge connected opaque region. A destructive spell, construction spell, door toggle, fixture placement, or temporary barrier should dirty only the chunks touched by the changed cells.

The live manager has one dirty chunk set. This is simple and bounded by chunk size, but it means a small dynamic change can still rebuild the ordinary rectangles in that chunk. A future version could keep separate ordinary and dynamic dirty sets, or even separate dynamic categories, so a small opacity change refreshes only the relevant output.

## Limitations

The manager does not generally know why a cell is opaque. It only stores a boolean plus the current door flag. If a future caller needs to distinguish stone walls, doors, constructed blockers, fog, smoke, glass, one-way occluders, spell fields, or temporary effects, the stored cell state will need to grow.

`ClearAll()` clears the internal grids and rectangle outputs, but it does not automatically repopulate from a map. After clearing, the caller must feed opacity again before rebuilding.

The manager assumes positive fixed dimensions. It is not a resizable structure. If a map is resized, make a new manager or add an explicit resize path.

## Staged Plan

Stage 1 is complete. The rectangle-only contract is locked down by `occluder-tests.fsx`, covering empty maps, single cells, merged runs, rectangular blocks, separated blocks, doors as the current named dynamic case, door toggles, ordinary mutable opacity changes, chunk boundaries, `ClearAll`, and `ForEachRectInView`.

Stage 2 is to make mutable occluder state less door-specific. The current door flag can stay as the first implementation, but the data model should be ready for other independently changing blockers: constructed walls, destroyed terrain, fixtures, smoke, fog, glass, force fields, and temporary spell effects. The first useful improvement is probably a small occluder-cell classification rather than more special-case booleans.

Stage 3 is to split dirty work by output kind if profiling or gameplay pressure justifies it. The current single dirty chunk set is bounded and simple. If frequently changing occluders become common, add separate dirty sets for ordinary merged rectangles and dynamic rectangles so small changes do not rebuild ordinary chunk geometry unnecessarily.

Stage 4 is to add explicit rebuild helpers for common map lifecycle events. `ClearAll()` can stay low-level, but callers would benefit from a clear way to repopulate the manager from a map after load, large edits, or wholesale terrain generation. If map resizing becomes a real workflow, add an explicit resize/recreate path rather than silently stretching the existing arrays.

Stage 5 is to keep the archived edge/inset approach archived unless rectangle output stops being enough. If that happens later, revive it as a separate design pass with tests, not as an accidental expansion of the rectangle manager.

## Current Status

The focused `ChunkOcclusionManager` tests are in place and passing as of 2026-07-06. No active occluder implementation task remains in this document.

## Source Map

- `ChunkOcclusionManager.fs` - current manager and live behavior.
- `occluder-tests.fsx` - focused rectangle-only contract tests.
- `Types.fs` - `TileOpacity` and opacity helpers.
- `LayerGrid.fs` - `LayerQueries.EffectiveTileOpacity`.
- `Maps.fs` - runtime `TileMap` and effective opacity cache.
- `chunkoccluder-inset.txt` - archived alternate edge/inset approach.

## History

### 2026-07-02

- Started the first occluder task note under `docs/`, capturing the current rectangle-based manager and the archived edge/inset alternative.

### 2026-07-03

- Reworked the note into a prose system document: live behavior, mutable environment updates, tradeoffs, limitations, source map, and the first test-focused occluder plan.
- Clarified the main design direction: keep rebuilds local by chunking merged geometry and by treating independently mutable occluders as their own update concern.
- Accepted rectangle-only output as the current plan and replaced the open-ended task note with a staged plan for tests, dynamic occluder state, dirty-work splitting, and lifecycle helpers.

### 2026-07-06

- Added focused `ChunkOcclusionManager` tests in `occluder-tests.fsx`.
- Marked Stage 1 complete: the current rectangle-only output contract, door special case, chunk-boundary behavior, layer-derived opacity, clearing, and view filtering are now covered by tests.
