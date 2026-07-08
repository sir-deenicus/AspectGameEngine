# 2D Engine, Maps, Layers, And Serialization

This document explains how the current 2D engine model works: sprite references, tiles, tilesets, runtime layers, runtime maps, editor maps, conversion between runtime and editor state, and FlatBuffers serialization.

The short version: AspectGameEngine is a data engine for a tile-based game and editor. It does not render the scene and it does not own frontend assets. It stores tile grids, sprite/entity references, layer placement, map metadata, exploration state, and small gameplay-facing helpers. Runtime maps are mutable and optimized for cheap updates during play. Editor maps are immutable and optimized for snapshots, undo/redo, and batch edits. Animation is intentionally out of scope for this document; see `docs/Animation.md`.

## Explanation

The engine is built around a flat rectangular grid. Tiles and layer cells are stored in one-dimensional arrays or persistent vectors, and coordinates are converted with the same convention throughout the map code:

```text
index = y * width + x
```

Runtime code uses `TileMap`. Editor code uses `EditorTileMap`. They share the same logical map shape, but not the same ownership model.

Runtime state is meant to be changed in place. Moving an actor, opening a door, setting a fixture, changing a tile, marking exploration, and recomputing effective opacity all mutate the `TileMap` instance.

Editor state is meant to be copied by value. Editing a tile, adding an item, resizing the map, or changing a layer cell returns a new `EditorTileMap` that reuses structure through `PersistentVector`. `EditorHistory` stores these immutable snapshots for undo and redo. The vector implementation is covered in `docs/Persistent-Vector.md`.

That split is the main design constraint. A runtime optimization should not make editor snapshots harder, and an editor convenience should not make runtime mutation allocate more than necessary.

## Core Grid Types

`GridPos` and `GridDelta` are small structs used for tile coordinates and movement deltas.

`SpriteLoc` is the older tile-oriented atlas coordinate:

```text
atlas index, row, column
```

Base tiles still use `SpriteLoc` as their identity inside a tileset. A map tile points to a `SpriteLoc`, and the active tileset resolves that sprite location to `TileProperties`.

Entity sprites use the newer `SpriteRef` union. A `SpriteRef` can refer to:

- `SheetRegion`: an arbitrary pixel region on a registered sheet.
- `SheetCell`: one grid cell on a registered sheet.
- `SheetSpan`: a rectangular block of sheet cells.
- `SheetCells`: an ordered list of irregular sheet cells.
- `TextureId`: an externally managed texture id.
- `Scene`: a frontend scene or prefab path.

The engine treats these as references, not loaded render resources. The Godot/front-end layer remains responsible for resolving ids, paths, atlases, materials, and draw calls.

## Tiles And Tilesets

A runtime `Tile` is per-cell instance state:

- `SpriteLoc`
- mutable `Health`
- `IsOccupied`

The tile does not directly store walkability, opacity, description text, biome, or interaction rules. Those come from `TileProperties`, looked up through the current tileset.

`TilePropertiesReference` is the tileset data structure. It maps `SpriteLoc` to `TileProperties`, and carries a `TileSetName` for persistence and registry lookup.

`TileProperties` currently records:

- `Walkable`
- `Interactable`
- `TileType`
- `Health`
- `DescriptionKey`
- `Biome`
- `TileOpacity`
- keyed `Visuals`
- legacy/convenience `DestroyedSpriteLoc` and `NextStateSpriteLoc`
- optional `ComplexState`

Missing tile properties resolve to `TileProperties.NullTile`. That default is non-walkable, opaque, uninteractive, and has no visuals.

`TilesetRegistry` is a simple process-local registry keyed by tileset name. Runtime map operations such as `GetTileProperties`, `IsWalkable`, and effective opacity recomputation need the map's `TileSetName` to be registered before they are called. `TileMap` constructors intentionally do not touch this registry, because deserialization and load order may construct maps before tilesets are available.

## Tile Opacity

`TileOpacity` is the shared opacity vocabulary for base tiles, fixtures, actors, FOV, and occlusion:

- `Opaque`: blocks sight.
- `Transparent`: does not block sight.
- `Air`: does not block sight and is used for open/empty space semantics.
- `Translucent`: visible itself, and may allow limited visibility through it in FOV.

The map's runtime visibility input is not just the base tile opacity. It is effective opacity: base tile opacity combined with the layer cell at that tile.

Actors and fixtures may supply opacity. Items and decals do not affect opacity. Actor opacity wins over fixture opacity, and fixture opacity wins over base tile opacity. If neither actor nor fixture provides opacity, the base tile opacity is used.

## Sprites And Entity Properties

Entity sprites are registered by integer id in `EntityRegistry.SpriteProps`. The id assignment is external to the engine.

Each `SpriteProperties` value contains:

- `Sprite`: the `SpriteRef` used by the frontend.
- `SpriteType`: the semantic kind and behavior data.
- `RenderLayer`: a draw-order hint.

`SpriteType` has four current cases:

- `Actor`: has opacity, description key, and optional `NpcFrames`.
- `Fixture`: has movement blocking, interactability, moveability strength, description key, and opacity.
- `Item`: has a description key.
- `Decal`: has interactability and a description key.

The registry is intentionally simple and global. Constructors avoid hidden registry work. Loading or replacing registry contents is explicit through `EntityRegistrySerializer.loadIntoModule`, direct dictionary writes, or caller-owned setup.

`NpcFrames` is minimal sprite selection data for two facings and two poses. It is not the full animation system. Current player helpers use it to swap the registered player actor sprite when facing or pose changes. Time-based animation, frame timing, and richer state machines are covered in `docs/Animation.md`.

Light sources are covered in `docs/Lights.md`. Visual effects such as particles, smoke, steam, sparks, fire, and magical fields are covered in `docs/Effects.md`. Layered sprite lighting shadows are covered in `docs/Sprite-Shadows.md`. Base tiles do not cast renderer lighting shadows; shadow-casting visual objects should be represented as layer sprites.

## Visual Shape, Entity Shape, And Sorting

`SpriteRef` can describe visuals larger than one tile. `SheetSpan`, `SheetCells`, `SheetRegion`, `TextureId`, and `Scene` can all point at art that is wide, tall, irregular, or arbitrary.

That visual shape is not collision shape.

The current runtime layer implementation still places actors and fixtures in one anchor cell. A future multi-cell entity pass should keep the rule simple: gameplay shape is tile-based only. There should be no runtime polygon collision, alpha masks, or fractional overlap checks in the engine.

The minimal intended shape is:

```fsharp
type EntityShape =
    { Body: GridDelta[]
      BaseOverride: GridDelta[] option }
```

`Body` is the set of map cells the entity meaningfully occupies when projected onto the tile grid. It is used for sight, projectile hits, click-to-attack targeting, visual cover hints, and other whole-body queries.

`Base` is the support or feet projection used for movement. It is usually derived from `Body`, not authored separately. With y increasing downward, the derived base is the lowest body cell in each occupied x column. `BaseOverride` exists for objects such as trees, where the canopy may be part of `Body` for sight or cover but only the trunk or roots should block movement.

For arbitrary art, the editor/importer should decide `Body` before runtime. The basic authoring rule is: if the visual covers at least half of a tile, include that tile in the body; if it covers less than half, treat it as visual overhang. The editor can still allow manual correction because authored taste matters for trees, tables, statues, rocks, and other uneven silhouettes.

The engine should then use existing semantics over these cells:

- Movement collision uses `Base`, plus the existing actor/fixture blocking rules.
- Vision and FOV use `Body`, plus the existing actor/fixture opacity rules.
- Projectiles and click-to-attack targeting use `Body`.
- Walk-to-attack uses grounded base contact: move until the attacker's base can contact the target's base, then roll the attack.
- Click-to-attack uses current-position body/reach overlap: project the attacker and target bodies into world cells, account for different y positions and heights, and roll the attack when the reached body overlap is sufficient.
- Renderer sorting uses `RenderLayer`, then a baseline derived from `Base`, then a stable tie-breaker such as local object id or placement order.

This keeps tall and wide entities expressive without making the engine a geometry simulator. A giant actor can have a tall body and a lower base. A tree can have a broad body and a trunk base override. A tall statue can block sight through its body while sorting by its base. A character walking behind a tree can be visually hinted by the renderer when covered by a body cell, without changing movement collision.

## Runtime Layers

Each runtime tile has one `LayerCell` parallel to the base tile array. A `LayerCell` can contain:

- `Items`: many item ids, capped by `EntityRegistry.MaxItemsPerTile`.
- `FixtureId`: one optional fixture id.
- `ActorId`: one optional actor id.
- `Decals`: a small fixed-capacity stack.
- `DecalCount`: active decal count.

Runtime item rendering is exposed through a non-allocating `ItemView`. `LayerQueries.GetRenderItemView` returns only the top `MaxRenderItemsPerTile` items by slicing the existing `ResizeArray`.

Runtime decals are stored bottom-to-top in an array. `LayerQueries.AddDecal` appends until `MaxDecalsPerTile`; when full, it replaces the current topmost decal. `GetDecal` returns the topmost decal id. `GetDecalView` exposes the active bottom-to-top stack.

Layer queries also compute effective opacity. The rule is:

```text
actor opacity, if present
else fixture opacity, if present
else base tile opacity
```

Items and decals are render/interaction layers, not light blockers.

## Runtime TileMap

`TileMap` is the mutable runtime map. It owns:

- `Width` and `Height`
- flat `Tiles`
- flat `LayerCells`
- flat `EffectiveOpacity`
- `EffectiveOpacityInitialized`
- `VoidSpriteLoc`
- `MapName`
- `MapType`
- `TileSetName`
- fixed-capacity `SpawnPoints`
- flat `Explored` flags

Out-of-bounds tile reads return a synthetic tile using `VoidSpriteLoc`. Out-of-bounds walkability is false. Out-of-bounds occupancy and opacity behave as blocked.

`TileMap.Update` changes a base tile and recomputes effective opacity at that cell. `SetFixture`, `ClearFixture`, `SetActor`, `ClearActor`, `TryMoveActor`, and `TryMoveFixture` also recompute effective opacity for the changed cell or cells.

`InitEffectiveOpacityCache` fills the whole `EffectiveOpacity` array from the registered tileset and all layer cells. It should be called once the tileset registry and entity registry are ready. It is safe to call multiple times.

The `EffectiveOpacityInitialized` flag records whether the whole cache has been initialized. Current individual mutation methods still write through to the cache for touched cells. Load/setup code should still initialize the full cache before systems such as FOV or occlusion depend on it.

`Explored` is persistent runtime map state. `TileMap.MarkExplored`, `IsExplored`, and `ClearExplored` operate on the flat byte array. Current visibility stamps are not stored here; only permanent exploration is.

## Runtime Movement And Interaction Helpers

The engine includes small gameplay-facing helpers, but they are still map/data helpers rather than a full game framework.

`TileMap.IsWalkable` reads base tile properties from the registered tileset. `TileMap.IsOccupied` checks the tile's own `IsOccupied` flag, then actor presence, then blocking fixtures. Actors are always treated as movement blockers. Fixtures block only if their registered fixture properties say so.

`TryMoveActor` validates bounds, walkability, and occupancy before moving the actor id between layer cells. `TryMoveFixture` validates fixture presence and, when the fixture blocks movement, validates the destination. `TryPushFixtureAndMoveActor` is the narrow atomic helper for the moveable push case: it moves the fixture forward and the actor into the fixture's old cell as one validated map mutation. `TrySwapActorAndFixture` is the narrow helper for movement rules that intentionally swap the player actor with a fixture in an adjacent cell, such as the moveable push-or-swap rule.

`GameState.fs` wraps a runtime map in `GameModel`, tracks player position and visibility settings, recomputes FOV, dispatches simple door interactions, and implements `lookAt`. `lookAt` combines the base tile description with the top three layer objects by `RenderLayer` and stable cell order. Door interactions now have typed result entry points returning `InteractionResult`, with the old `InteractResult` wrappers retained for existing callers.

`Player.fs` exposes `tryMove` as the typed movement API returning `MovementResult`. `tryMoveBool` is the explicit compatibility wrapper for callers that still need a boolean. The movement path returns stable message keys, updates facing and pose sprite references through the registry, auto-opens a door on the destination tile when possible, and recomputes visibility after successful movement.

`EngineChangeSet` is the shared mutation hint shape used by current movement and door interaction results. It reports changed base cells, changed layer cells, changed entities by slot, whether visibility/FOV should refresh, whether occlusion inputs changed, and whether the mutation is save-relevant. Movement always marks visibility changed on success because the player/FOV origin moved. It marks occlusion changed only when the effective opacity grid changes, such as an opaque fixture moving or a door tile changing; transparent movement still reports layer/entity changes without forcing an occlusion rebuild.

`Objects.fs` contains moveable fixture helpers. It checks a local moveable-object dictionary first, then falls back to registered fixture `Moveable` strength. The helper rule is push-forward-else-swap, matching `docs/Interactions.md`. The Stage 1 player-movement integration is verified by `movement-tests.fsx`.

The interaction direction is covered in `docs/Interactions.md`. Interactables should be agnostic to whether they are represented by a base tile or a layered sprite, and live interaction state should be map-local rather than stored on reusable registry definitions.

## Editor Layers

`EditorLayerCell` is the immutable editor counterpart to `LayerCell`. It contains:

- `Items: int list`
- `FixtureId: int option`
- `ActorId: int option`
- `Decals: int list`

Editor items and decals are stored top-first, with the newest/topmost id at the list head. `EditorLayerQueries.GetRenderItems` and `GetRenderDecals` truncate from the head.

This is intentionally different from runtime decal storage. Runtime decals are bottom-to-top; editor decals are top-to-bottom. Conversion reverses decal order so the visual stack survives round trips.

`EditorLayerQueries.EffectiveTileOpacity` uses the same actor-over-fixture-over-base rule as runtime layer queries.

## EditorTileMap

`EditorTileMap` is an immutable map snapshot. It owns:

- `Width` and `Height`
- flat `PersistentVector<Tile>`
- `VoidSpriteLoc`
- `MapName`
- `MapType`
- `TilesetName`
- flat `PersistentVector<EditorLayerCell>`
- spawn points as a list

`EditorTileMap.New` requires positive initial dimensions. `Resize` can produce zero dimensions for intermediate editor states, but negative dimensions fail.

Tile edits, layer edits, spawn-point edits, metadata edits, and batch updates return a new `EditorTileMap`. Batch tile updates and batch layer-cell updates use `PersistentVector.updateManyWith` so large brush operations can share unchanged structure.

Resize preserves existing rows and columns where possible, trims removed cells, and fills new cells with the map's `VoidSpriteLoc` tile and empty editor layer cells.

`SetEntityAuto` chooses the target layer from the registered entity's `SpriteType`: actors go to the actor slot, fixtures to the fixture slot, decals to the decal stack, and items to the item stack. Unknown entity ids are ignored.

`MigrateEntitySpriteType` is the editor-side rule for changing an existing entity definition. If an entity crosses the item/non-item boundary, existing placement is removed. If it moves between non-item kinds, the editor tries to place it in the new compatible slot if that slot is open; otherwise the old placement is removed.

`EditorHistory` stores editor map snapshots. The newest snapshot is at `CurrentIndex = 0`. Undo moves toward older entries, redo moves toward newer entries, and new edits truncate redo history.

## Runtime And Editor Conversion

`EditorTileMap.FromTileMap` copies runtime tiles into a persistent vector and converts runtime layer cells into editor layer cells.

Conversion rules:

- Tiles are copied as tile values.
- Runtime item `ResizeArray` becomes an F# list.
- Fixture and actor ids are copied directly.
- Runtime decals are read bottom-to-top and written top-first into the editor list.
- Runtime spawn points are filtered to valid coordinates.

`EditorTileMap.ToTileMap` converts editor vectors back to runtime arrays and constructs a new `TileMap`.

Conversion back to runtime:

- Tiles become a flat array.
- Editor items become a runtime `ResizeArray`.
- Fixture and actor ids are copied directly.
- Editor decals are reversed, then appended through `LayerQueries.AddDecal`, restoring runtime bottom-to-top order.
- Editor spawn points become the runtime spawn-point array.

The new runtime map still follows the constructor rule: it does not initialize effective opacity by reading the tileset registry. Callers that need FOV, occlusion, or opacity queries should initialize the effective opacity cache after conversion once registries are ready.

Exploration is not currently part of `EditorTileMap`. Runtime-to-editor conversion does not carry `TileMap.Explored`, and editor-to-runtime conversion creates a fresh runtime map with a fresh explored array.

## Serialization

Serialization is split into three FlatBuffers streams:

- Tileset properties: `TilePropertiesSerializer.fs` and `tile_defs.fbs`.
- Runtime maps: `MapTypeSerializer.fs` and `map_defs.fbs`.
- Entity registry data: `EntityRegistrySerializer.fs` and `entity_registry.fbs`.

Generated C# FlatBuffers code lives under `FlatBufferTypes/Generated` and should not be edited by hand.

## Tileset Serialization

`TilePropertiesSerializer.serialize` writes a `TilePropertiesReference` as a `TilePropertiesSetFBS`.

The key is `SpriteLoc`; the value is `TileProperties`. Serialized properties include walkability, interactability, tile type, health, description key, biome, tile opacity, visual entries, optional legacy visual locations, and optional complex state.

Visuals are now the general representation. The serializer still merges legacy `DestroyedSpriteLoc` and `NextStateSpriteLoc` into keyed visual entries using `"destroyed"` and `"next_state"` when those keys are missing. Deserialization reads both the visual array and the legacy fields, then derives the legacy options from keyed visuals when present.

Current complex state serialization supports `ClosedDoorStateFBS`. The union is extensible, but adding a new case requires updating the schema, generated code, mapper functions, tests, and backward-compatibility notes.

## Map Serialization

`TileMapSerializer.serialize` writes only runtime map data, not tileset definitions and not entity definitions.

The map stream stores:

- dimensions
- flat tile array
- flat layer-cell array
- `VoidSpriteLoc`
- map name
- map type
- tileset name
- spawn points
- explored flags

Layer cells serialize item ids, fixture id, actor id, legacy single decal id, and the newer decal vector. The decal vector is bottom-to-top. Deserialization prefers the vector and falls back to the legacy single decal id when the vector is absent.

Spawn points are written as exactly ten entries. Missing or shorter older data is tolerated on load and padded with `(-1, -1)`.

Explored flags are serialized as a flat byte vector. Missing or shorter older data is tolerated on load; only the overlapping prefix is copied.

Effective opacity is not serialized. It is derived state, rebuilt from the loaded map, registered tileset, and registered entity properties.

## Entity Registry Serialization

`EntityRegistrySerializer.serializeFrom` writes a supplied dictionary of sprite properties. `serializeCurrent` writes the live global `EntityRegistry.SpriteProps`.

The registry stream groups entries by semantic type:

- items
- fixtures
- actors
- decals

Each entry stores its integer id, `SpriteRef`, render layer, and type-specific properties. Actors may include `NpcFrames`.

`deserialize` returns `RegistryData` without mutating the live module. `loadIntoModule` clears `EntityRegistry.SpriteProps` and loads the deserialized entries into the global registry. Use the non-mutating path when callers need staging, validation, or custom ownership.

## Visibility And Occlusion Fit

FOV reads `TileMap.GetOpacityByIndex`, which reads `TileMap.EffectiveOpacity`. The FOV system does not inspect tilesets or layer cells directly. This keeps visibility focused on geometry and opacity, while map/layer code owns how effective opacity is derived.

Exploration belongs to `TileMap`, not to `VisibilityState`. FOV stamps temporary current visibility in `VisibilityState` and marks persistent exploration on the map.

`ChunkOcclusionManager` has its own opacity grid and chunked rectangle cache. It can be populated from base tile opacity plus a layer cell with `SetFromTileAndLayer`, while door cells can be driven separately by `SetDoorCellState`. Doors are excluded from merged wall rectangles and become separate `1x1` occluders when opaque.

## Performance Notes

The runtime map uses arrays, mutable cells, and direct indexing. Layer queries avoid allocations where they are on rendering or interaction paths. `ItemView` and `DecalView` expose slices/views over existing storage instead of building fresh lists.

Effective opacity is cached because FOV and occlusion need cheap per-tile opacity reads. Mutating tile or relevant layer state should update the affected cache entries; bulk load or conversion should initialize the whole cache once registries are ready.

The editor map uses persistent vectors so normal editing can create snapshots without copying every unchanged tile or cell. Batch updates exist for brush-like operations where many cells change together.

The map serializer writes compact FlatBuffers arrays in flat grid order. It does not attempt to delta-compress maps or deduplicate repeated tiles.

## Design Boundaries And Operating Rules

The engine is deliberately not a renderer. That is a strength: maps, layers, entity placement, serialization, visibility, and gameplay helpers can be reused across frontends, upgraded independently, and paired with different visual systems without rewriting the engine data model. `SpriteRef`, `SpriteLoc`, `RenderLayer`, and `NpcFrames` are data contracts for a frontend, not draw code.

The engine does not own asset lifetime. Sheet ids, texture ids, scene paths, localization keys, and entity ids must be meaningful to the caller.

`TilesetRegistry` and `EntityRegistry.SpriteProps` are global mutable registries. This is simple and convenient, but load order matters. Constructors avoid hidden registry lookups, and callers should explicitly register data before using property, walkability, occupancy, opacity, FOV, or occlusion queries.

`TileMap.IsWalkable` uses base tile properties. Blocking actors and fixtures are handled by `IsOccupied`, not by changing tile walkability.

`TileMap.IsOccupied` assumes registered fixture ids can be looked up when present. Unknown fixture ids may fail through direct dictionary indexing in runtime occupancy and movement paths.

Multi-cell visual references do not imply multi-cell collision. `SpriteRef` is only a visual/input reference. Any future wide or tall gameplay entity should use an explicit tile-based `EntityShape`, with movement based on the derived base and sight/projectile/click targeting based on the body.

`EditorTileMap` does not carry exploration state. Converting runtime maps into editor maps and back is a content-editing round trip, not a full runtime-state preservation round trip.

`TileMap.ToTileMap` conversion from editor state does not initialize effective opacity. That remains an explicit post-conversion/load step.

Door behavior is currently simple and visual-state driven. Door interactions use the first visual entry of the current tile properties as the target state. Lock/unlock data exists in types, but the current interaction path only handles open/close. The current live state can remember a locked bool by tile index; it does not yet store lock ids, check player inventory, or support item-definition capability checks for visually identical items with different gameplay payloads.

The intended save direction for gameplay progress is a live-state overlay over authored map/content data. The map serializer remains content/map serialization; future save state should record changed door open/lock facts, moved objects, player inventory, container inventories, picked-up or dropped item stacks, trigger state, and reversible tile swaps without mutating shared definitions.

Map serialization is backward-aware for spawn points, explored flags, and legacy single decals, but every new serialized field still needs an explicit compatibility decision and tests.

## Source Map

- `Types.fs` - grid structs, sprite references, actor visual structs, tile enums, opacity, map type, complex tile state, tile visuals, and tile properties.
- `LayerGrid.fs` - entity sprite properties, global entity registry, runtime layer cells, runtime/editor layer queries, opacity composition, and editor layer cells.
- `Maps.fs` - tileset registry, runtime tile properties reference, mutable runtime `TileMap`, layer mutation, movement, actor/fixture swap, occupancy, effective opacity cache, and exploration.
- `MapEditor.fs` - immutable `EditorTileMap`, editor resize/update APIs, entity placement/migration, runtime/editor conversion, and `EditorHistory`.
- `PersistentVector.fs` - local persistent vector implementation used by editor maps.
- `TilePropertiesSerializer.fs` - FlatBuffers tileset serialization and deserialization.
- `MapTypeSerializer.fs` - FlatBuffers runtime map serialization and deserialization.
- `EntityRegistrySerializer.fs` - FlatBuffers entity registry serialization and deserialization.
- `GameState.fs` - game-facing wrapper around runtime maps, player model, typed door interaction results plus legacy wrappers, look-at, and visibility recomputation.
- `Player.fs` - typed player movement result API, boolean movement wrapper, and visual sprite synchronization.
- `Objects.fs` - moveable fixture helper code.
- `docs/Interactions.md` - interaction targeting, map-local interaction state, containers, triggers, and moveables.
- `docs/Effects.md` - visual effects, particle profile keys, anchors, lifetimes, optional linked lights, and frontend handoff.
- `docs/Lights.md` - light sources, anchors, time-linked sun/moon/window light, and frontend handoff.
- `ChunkOcclusionManager.fs` - chunked opaque-cell and occluder-rectangle tracking.
- `FlatBufferTypes/Schemas/tile_defs.fbs` - tileset schema.
- `FlatBufferTypes/Schemas/map_defs.fbs` - map schema.
- `FlatBufferTypes/Schemas/entity_registry.fbs` - entity registry schema.
- `map-tests.fsx` - map serialization, layer round trips, decal ordering, spawn points, and editor conversion tests.
- `entity-registry-test.fsx` - entity registry serialization tests.
- `tests.fsx` - broader engine behavior tests.

## Historical Notes

### 2026-07-07

- Recorded the Stage 1 movement API shift: `Player.tryMove` now returns `MovementResult`, while `Player.tryMoveBool` preserves the boolean wrapper shape.
- Recorded `TileMap.TrySwapActorAndFixture` as the narrow map mutation helper for the moveable push-or-swap rule.
- Updated runtime movement helper notes to say `Objects.fs` follows push-forward-else-swap.
- Recorded the Stage 1 verification-boundary update: `TileMap.TryPushFixtureAndMoveActor` makes push-plus-player movement atomic, `Player.tryMove` returns stable movement message keys, and focused movement tests cover the intended edge cases.
- Completed Stage 1 verification: focused movement tests and broad regression tests pass, including moved actor/fixture serialization coverage.
- Clarified current door lock limitations and the intended save-game live-state overlay model for moved objects, door state, inventory/container state, item stack changes, triggers, and tile swaps.
- Completed Stage 2 rendering-foundation support: `InteractionResult` now carries typed door interaction changes, legacy interaction wrappers remain, and movement occlusion hints are based on effective-opacity changes instead of every successful move.

### 2026-07-06

- Added the intended multi-cell entity shape direction.
- Clarified that `SpriteRef` can describe large or arbitrary visuals, but visual size does not imply gameplay footprint.
- Chose a minimal tile-based shape model: `Body` cells plus an optional base override, with the ordinary base derived from the lowest body cell in each occupied column.
- Recorded that runtime collision remains tile-based; no masks, polygons, or fractional geometry belong in the engine hot path.
- Split system use of shape: movement and walk-to-attack use base contact, while vision, projectiles, click-to-attack, and cover hints use body cells.
- Recorded renderer sorting as render layer plus base-derived baseline plus stable tie-breaker.

### 2026-07-05

- Added the first live 2D engine document.
- Captured the runtime/editor split as the central map ownership rule: `TileMap` is mutable runtime state, while `EditorTileMap` is immutable editor state.
- Documented the current layer model: one actor, one fixture, many items, fixed-capacity decals, actor-over-fixture-over-base opacity, and the runtime/editor decal ordering conversion.
- Documented the three FlatBuffers persistence streams and the important derived-state rule: effective opacity is rebuilt, not serialized.
- Recorded current design boundaries around global registries, editor exploration ownership, explicit opacity-cache initialization, and simple door interaction semantics.
