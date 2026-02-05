# AspectGameEngine

AspectGameEngine is an F# (`net8.0`) library of core data structures used by a tile-based game (and its editor): tiles + tilesets, layered entities, an immutable editor map with undo/redo, FlatBuffers serializers, and a chunked occlusion/visibility helper.

This repo is primarily *data + algorithms* (not a renderer). It’s designed so you can:
- Keep a fast, mutable runtime `TileMap` for gameplay.
- Keep an immutable editor `EditorTileMap` that supports cheap snapshots and history.
- Serialize/deserialize maps and registries via FlatBuffers.

## Project layout

The library is a single F# project: `AspectGameEngine.fsproj`.

Key source files:
- `Types.fs`: foundational types (`SpriteRef`, `TileProperties`, `TileOpacity`, `MapType`, etc.).
- `LayerGrid.fs`: per-tile “layer” data (actors/fixtures/items/decals) for runtime + editor.
- `Maps.fs`: the runtime `TileMap` and the `TilesetRegistry`.
- `MapEditor.fs`: immutable `EditorTileMap`, resize/batch edit helpers, and undo/redo (`EditorHistory`).
- `PersistentVector.fs`: an in-repo persistent vector (Clojure-style) used heavily by the editor map.
- `ChunkOcclusionManager.fs`: chunked, greedy-merged occluder rectangles derived from tile opacity.
- `TilePropertiesSerializer.fs`, `MapTypeSerializer.fs`, `EntityRegistrySerializer.fs`: FlatBuffers serialization helpers.
- `Localization.fs`: localization helpers and data.
- `SpellsCore.fs`: core spell-related types/helpers (game-specific).

## Core concepts

### 2D maps stored as flat arrays/vectors
Both runtime and editor maps store `Width`×`Height` data as a single 1D sequence using:

`index = y * width + x`

This keeps memory contiguous and simplifies serialization and bulk operations.

### “Layer cells” (actors/fixtures/items/decals)
Each tile has a logical layer cell that can hold:
- `ActorId: int option`
- `FixtureId: int option`
- `DecalId: int option`
- `Items: int list` (editor) or `ResizeArray<int>` (runtime)

The actual entity metadata comes from `EntityRegistry.SpriteProps` (a dictionary keyed by entity id).

## Focus areas

### `LayerGrid.fs` (tile layers)

Runtime-side:
- `LayerCell`: mutable container per tile (`Items` is a `ResizeArray<int>`).
- `ItemView`: non-allocating slice view over the “top N” items for rendering.
- `LayerQueries`:
  - `GetRenderItemView` / `CopyRenderItemIds`: pull the top `EntityRegistry.MaxRenderItemsPerTile`.
  - `EffectiveTileOpacity`: combines base tile opacity with actor/fixture opacity (items don’t affect opacity).

Editor-side:
- `EditorLayerCell`: immutable version of `LayerCell` (items are an immutable `int list`).
- `EditorLayerQueries`: editor equivalents, including `EffectiveTileOpacity`.
- `EditorLayerCellUpdate`: update payload used by batch update APIs.

Why there are two representations:
- Runtime prefers mutation and low allocations.
- Editor prefers structural sharing and fast snapshots.

### `Maps.fs` (runtime map)

`TileMap` is the runtime, mutable map type:
- Tiles: `Tile[]` (flat).
- Layer cells: `LayerCell[]` (flat).
- Tileset metadata: `TileSetName` (used to look up properties).

Important helpers:
- `GetTile` returns the “void” tile if out-of-bounds.
- `GetTileProperties` pulls `TileProperties` from the registered tileset.
- Layer operations:
  - `AddItem`, `SetActor`, `SetFixture`, `SetDecal` + clears/queries.
- Gameplay queries:
  - `IsWalkable`: from tile properties.
  - `IsOccupied`: base tile + actor + blocking fixture.
  - `IsOpaque`: base tile opacity overridden by actor/fixture opacity.

Tileset support:
- `TilePropertiesReference`: dictionary keyed by `SpriteLoc`.
- `TilesetRegistry`: global registry of tilesets (`register`, `tryGet`, `get`).

### `MapEditor.fs` (immutable editor map + undo/redo)

`EditorTileMap` is an immutable editing surface:
- Tiles: `PersistentVector<Tile>`
- Layer cells: `PersistentVector<EditorLayerCell>`
- Same coordinate model as `TileMap` (flat index).

Key editing APIs (all return a new map):
- `UpdateTile`, `BatchUpdate`
- `SetActor` / `ClearActor`, `SetFixture` / `ClearFixture`, `SetDecal` / `ClearDecal`, `AddItem`
- `SetEntityAuto`: decides actor/fixture/decal/item placement based on `SpriteType` from the entity registry.
- `UpdateLayerCell` and `UpdateLayerCells` (batch)
- `Resize`: preserves existing data where possible; fills new space with empty tile/cell defaults.
- Conversion:
  - `EditorTileMap.FromTileMap` (runtime → editor)
  - `EditorTileMap.ToTileMap` (editor → runtime)

Undo/redo:
- `EditorHistory` stores a bounded list of `EditorTileMap` snapshots.
- `Undo`/`Redo` are index shifts; edits push a new snapshot and truncate redo history.

Why `PersistentVector` matters here:
- Large maps can be snapshotted efficiently because vectors share structure.
- Batch edits use transient mutation under the hood (see below) and then “freeze”.

### `ChunkOcclusionManager.fs` (chunked occlusion rectangles)

`ChunkOcclusionManager` maintains a boolean “opaque grid” and produces rectangles for occlusion geometry:
- The map is split into chunks of `chunkSize`.
- When a cell changes opacity, only the affected chunk is marked dirty.
- `RebuildDirty()` greedily merges opaque *non-door* cells into larger rectangles per chunk.
- Doors are special-cased:
  - `SetDoorCellState` marks a cell as a door and sets its opacity from a `TileOpacity`.
  - Closed doors (opaque door cells) become dedicated 1×1 rects (not merged), so they can toggle cleanly.
- `ForEachRectInView` iterates merged rects + door rects over a view window.

Intended usage:
- Feed it effective opacity from `TileMap.IsOpaque`/`LayerQueries.EffectiveTileOpacity` (or equivalent).
- Rebuild only the chunks that changed, then use the rects to build/update occluder meshes.

### `PersistentVector.fs` (persistent vector + transient editing)

This file defines a Clojure-style persistent vector in the `FSharpx.Collections` namespace (implemented locally in this repo).

High-level behavior:
- `PersistentVector<'T>` is immutable:
  - `Conj` appends, `Update` replaces, `Take` truncates, etc. (returns a new vector each time).
  - Most operations share internal structure (tree nodes + tail array).
- `TransientVector<'T>` is a mutable “editing session”:
  - Used internally for efficient bulk operations, then converted back via `Persistent()`.
  - Ownership is enforced via an `EditSessionId` marker on nodes.

Useful surface API (from `module PersistentVector`):
- Construction: `empty`, `singleton`, `ofSeq`, `init`
- Queries: `isEmpty`, `length`, `nth`, `tryNth`, `head`/`tryHead`, `last`/`tryLast`
- Updates: `conj`, `update`, `updateAt`, `initial`, `unconj`
- Batch: `updateMany`, `updateManyWith`, `updateManyWithIndexMap`
- Transform: `append`, `map`, `mapi`, `filter`, `choose`, `concat`, etc.

## Serialization (FlatBuffers)

The project references `Google.FlatBuffers` and a generated types project: `FlatBufferTypes/AspectGameEngine.FlatBufferTypes.csproj`.

The `.fsx` scripts include examples:
- `map-tests.fsx`: serialize/deserialize `TileMap` and validate tile + layer cell data.
- `entity-registry-test.fsx`: serialize/deserialize the entity registry.

## Build & run

Build the library:
```powershell
dotnet build -c Debug
```

Run the F# scripts (they reference the built DLL under `bin/Debug/net8.0`):
```powershell
dotnet fsi .\tests.fsx
dotnet fsi .\map-tests.fsx
dotnet fsi .\entity-registry-test.fsx
```

### Post-build copy note
`AspectGameEngine.fsproj` includes a post-build step that copies the built DLL/PDB/XML into:
`..\aspectrpg\Scripts\GameEngine\`

If that destination directory doesn’t exist, the build will error. Create the folder, or remove/adjust the `PostBuild` target in `AspectGameEngine.fsproj` for standalone use.
