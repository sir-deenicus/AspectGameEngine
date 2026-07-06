# Persistent Vector

This document explains the local persistent vector implementation used by the editor map. The implementation lives in `PersistentVector.fs`.

The short version: this is a Clojure-style immutable vector with a 32-way tree, a tail array, and an internal transient editing mode. It gives `EditorTileMap` cheap snapshots and efficient random access without copying the whole tile grid for every edit. Runtime maps still use mutable arrays; the persistent vector is an editor data structure.

## Why It Exists

`EditorTileMap` needs value-style editing. A brush stroke, layer edit, resize, or metadata change should produce a new map snapshot so undo/redo can keep old versions alive. Copying a whole array on every edit would be simple, but expensive for large maps. A linked list would snapshot cheaply, but random tile access would be too slow.

`PersistentVector<'T>` sits between those extremes:

- indexed reads are effectively constant for current map sizes because the tree branches by `32`
- single updates copy only the path to one leaf plus the tail when needed
- appends are cheap while the tail has room
- batch edits use a transient vector so many updates can share one mutable editing session before returning to immutable state

That makes it a good fit for editor tiles and editor layer cells.

## Data Shape

The vector stores elements in a flat logical index space, but physically it is split into:

- `Count`: the number of logical elements.
- `Shift`: the current tree depth, in `5`-bit steps.
- `Root`: the root node of a 32-way tree.
- `Tail`: the final block of up to `32` elements.

The constants are:

```text
block size       = 32
block size shift = 5
block index mask = 0x01f
```

Indexing uses the low five bits to find a position inside a block and higher groups of five bits to walk the tree.

For small vectors, the tail can hold everything. When the tail fills, the old tail becomes a leaf node in the tree and a new tail starts. When the tree fills at the current depth, the root grows one level taller.

## Nodes And Ownership

Each `Node` owns an `obj[]` of size `32`. The vector stores values as `obj` internally, then casts back to `'T` on reads.

Nodes also carry an `EditSessionId` reference cell. This is the ownership mechanism for transients:

- a node whose `EditSessionId` value is `null` is persistent/frozen
- a node whose `EditSessionId` value is non-null belongs to an active transient session
- nodes owned by the same transient share the same reference cell, not merely the same marker value

That reference-cell identity check is important. A transient can mutate nodes it owns in place. If it reaches a persistent node, or a node from another transient, it copies that node first and then mutates the copy.

When a transient is finalized with `Persistent()`, it sets its shared owner reference cell to `null`. All nodes that belonged to the transient become persistent by the same shared-cell rule.

## Persistent Operations

`PersistentVector<'T>` is the immutable surface. The main operations are:

- `Conj`: append one value.
- `Update`: replace a value at an index, with the current special case that `index = Length` appends.
- `Initial`: remove the last value.
- `Last` and `Unconj`: read or split off the final value.
- `Take`, `drop`/`skip`, `append`, `concat`, `map`, `filter`, `choose`, and conversion helpers.

Persistent updates create fresh nodes along the modified tree path and share everything else. Tail updates copy the tail array. Appending into a non-full tail copies the tail and adds the new value.

## Transient Operations

`TransientVector<'T>` is internal. It exists so bulk edits do not allocate a new path for every update.

`PersistentVector.AsTransient()` creates a fresh edit session over an existing vector. The transient copies the tail into a full-size mutable buffer and lazily copies tree nodes only when it needs to write through them.

Batch APIs use this path:

- `PersistentVector.updateMany`
- `PersistentVector.updateManyWithIndexMap`
- `PersistentVector.updateManyWith`
- `PersistentVector.ofSeq`
- `PersistentVector.init`
- `PersistentVector.append`
- `PersistentVector.concat`

The important rule is that transients are single-use. After `Persistent()` is called, the transient must not be used again.

## Editor Map Use

`EditorTileMap` stores:

- `Tiles: PersistentVector<Tile>`
- `LayerCells: PersistentVector<EditorLayerCell>`

Single tile and layer edits call `Update`. Brush-like operations call `updateManyWith`. Resizing uses `take`, `skip`, `append`, `concat`, and `init` to preserve cells where possible and fill new cells.

`EditorHistory` stores whole `EditorTileMap` snapshots. Because tile and layer vectors share unchanged structure, history does not need to deep-copy every cell on each edit.

Runtime `TileMap` does not use this structure. Runtime state is mutable and array-backed because movement, opacity updates, exploration, and gameplay systems want direct in-place mutation.

## Serialization

The persistent vector itself is not serialized as a custom data structure.

Editor maps convert to runtime maps before using the map serializer. `EditorTileMap.ToTileMap` turns persistent vectors into arrays. `TileMapSerializer` then writes flat runtime map arrays in grid order.

Runtime-to-editor conversion goes the other direction: `EditorTileMap.FromTileMap` copies runtime arrays into persistent vectors using `PersistentVector.ofSeq`.

This keeps the persistent vector an editor implementation detail rather than a file-format commitment.

## Correctness And Speed Assessment

The implementation is fit for the current editor use. `tests.fsx` has older broad-engine coverage for append/remove behavior at `32`, `33`, `1024`, and `1025`, plus batch update helpers used by `EditorTileMap`. `persistent-vector-tests.fsx` is the dedicated suite for this structure: construction, indexing, immutability, append/update, `Initial`, `Unconj`, slicing, transforms, one-shot sequence inputs, duplicate batch updates, and randomized model checks against arrays.

There are no obvious stop-ship correctness issues in the current editor paths. The main improvement area is hardening the general collection contract and trimming avoidable work in lazy-sequence and resize-heavy paths.

The current strengths:

- the runtime/editor split is preserved
- persistent vectors are immutable from the public editor-map point of view
- transient sessions copy persistent nodes before mutation
- batch updates avoid repeated persistent path allocation
- failed transient batch updates do not mutate the original vector because the original persistent structure is copied on write

The current pressure points:

- `Take` can keep unreachable old tree branches alive after a large shrink
- width-changing editor resize currently builds row slices through `skip` plus `take`, and `skip` rebuilds the suffix before `take` trims it
- the raw `Node` type and raw `PersistentVector` constructor are publicly visible even though normal callers should use the module functions
- `Update(index = Length)` appends, while transient `UpdateInPlace` is strict; that split should be intentionally documented or tightened
- the vector tests are broad-engine tests, not a dedicated randomized collection suite

## Design Boundaries And Operating Rules

Use persistent vectors for editor snapshots, not runtime maps.

Prefer `updateManyWith` or `updateMany` for brush-like edits. Repeating `Update` in a loop creates more intermediate vectors than necessary.

Use sequential iteration when walking many elements. Random indexing is cheap for a persistent structure, but a full indexed loop still repeats tree navigation.

Treat transient vectors as internal and single-use. They are not thread-safe, and using one after `Persistent()` is a bug.

Treat vector elements as stable values if vectors are used as dictionary keys or compared by hash. The vector caches its hash code; mutating an element after the hash is computed can violate ordinary .NET hash/equality expectations.

Do not rely on the raw constructor or `Node` shape outside this file. The public module functions are the intended construction surface.

## Future Work

Correctness hardening:

- Keep expanding `persistent-vector-tests.fsx` when adding collection operations. New operations should be checked against arrays or lists, including boundary sizes around powers of `32`.
- Decide the `Update(index = Length)` contract. If append-on-update is intentional, document and test it directly. If not, make `Update` strict and require `Conj` for append.
- Hide the raw implementation surface if downstream code allows it. `Node` and the raw `PersistentVector` constructor should be internal or otherwise protected so invalid trees cannot be constructed outside the module.

Speed and memory work:

- Add a `slice` or `copyRange` helper that builds a vector from `RangedIterator(start, end)` without first constructing a dropped suffix. Use it in `EditorTileMap.Resize` for width-changing row copies.
- Optimize `toArray` with block-aware copying instead of going through `Seq.toArray`.
- Replace `foldBack`'s current `Seq.toList` allocation with an indexed reverse loop or block-aware reverse fold.
- Consider pruning or rebuilding after large `Take` operations so shortened maps do not retain unreachable branches from much larger old vectors.
- Extend `persistent-vector-bench.fsx` with editor-shaped benchmarks for map creation, brush updates, width resize, height resize, runtime/editor conversion, and undo-history snapshot pressure.

## Source Map

- `PersistentVector.fs` - `Node`, internal `TransientVector<'T>`, public `PersistentVector<'T>`, and the `PersistentVector` helper module.
- `MapEditor.fs` - `EditorTileMap` use of persistent vectors for tiles and layer cells.
- `tests.fsx` - current boundary and batch-update coverage.
- `persistent-vector-tests.fsx` - dedicated vector correctness and randomized model coverage.
- `persistent-vector-bench.fsx` - deterministic local speed benchmark for construction, reads, updates, batch updates, append, concat, and array conversion.
- `docs/2D-Engine.md` - broader map, layer, editor, and serialization context.

## Historical Notes

### 2026-07-05

- Added the first live persistent vector document.
- Captured the editor ownership role: immutable `EditorTileMap` snapshots backed by structural sharing, separate from mutable runtime `TileMap` arrays.
- Documented the 32-way tree, tail array, transient editing session, and serializer boundary.
- Reviewed correctness and speed pressure points. No urgent current-editor blocker found, but future work should still address constructor visibility, update semantics, resize slicing, `toArray`, `foldBack`, and large-shrink memory retention.
- Added `persistent-vector-tests.fsx` for dedicated correctness coverage, including one-shot sequence inputs and randomized model checking against arrays.
- Added `persistent-vector-bench.fsx` for deterministic local speed checks.
- Hardened `ofSeq`, `concat`, `updateMany`, `updateManyWithIndexMap`, and `updateManyWith` so they do not enumerate arbitrary input sequences twice.
