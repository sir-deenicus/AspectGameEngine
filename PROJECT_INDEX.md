# Project Index

Start here when orienting yourself in AspectGameEngine.

## Agent Entry Points

- `AGENTS.md` - repository instructions for coding agents.
- `PROJECT_ETHOS.md` - project-wide engineering principles.
- `docs/triage.md` - active engine slice triage and ordering.
- `docs/todo.md` - active engine implementation checklist.
- `docs/Map-Migrations.md` - map format versions, sequential migrations, legacy compatibility, and explicit disk upgrades.
- `docs/Occluder-Task.md` - chunk occluder behavior, test coverage, and future work.
- `Readme.md` - older overview; useful, but scheduled for cleanup.

## Current Task

The active task is tracked in `docs/triage.md`: durable engine support for the Light, Lock, And Look slice, including moveables, pickup/drop, containers, inventory, keys, locks, nearby look data, rock-on-rune reversible wall-to-floor tile swaps, localization keys, visibility/occlusion coherence, and serialization tests.

Completed or active docs from this pass:

- `docs/2D-Engine.md`
- `docs/Animation.md`
- `docs/Containers.md`
- `docs/Effects.md`
- `docs/Interactions.md`
- `docs/Lights.md`
- `docs/Localization.md`
- `docs/Map-Migrations.md`
- `docs/Persistent-Vector.md`
- `docs/Sprite-Shadows.md`

Pending docs from this pass:

- None. The documentation pass docs are now drafted.

The occluder rectangle-only contract is now covered by `occluder-tests.fsx`. Canonical occluder doc: `docs/Occluder-Task.md`.

## Documentation Layout

Root docs:

- `AGENTS.md`
- `PROJECT_ETHOS.md`
- `PROJECT_INDEX.md`

Technical docs and task notes:

- `docs/triage.md`
- `docs/todo.md`
- `docs/2D-Engine.md`
- `docs/Animation.md`
- `docs/Containers.md`
- `docs/Effects.md`
- `docs/Interactions.md`
- `docs/Lights.md`
- `docs/Localization.md`
- `docs/Map-Migrations.md`
- `docs/Persistent-Vector.md`
- `docs/Sprite-Shadows.md`
- `docs/Occluder-Task.md`
- `docs/Fog-Of-War.md`

No design-doc tree exists here. Keep `docs/` technical unless the project direction changes.

## Code Map

- `AspectGameEngine.fsproj` - F# project file, compile order, FlatBuffers package, generated-types project reference, post-build copy.
- `Types.fs` - foundational grid, sprite, tile, opacity, biome, map type, and tile-property types.
- `LayerGrid.fs` - entity sprite properties, global entity registry, runtime layer cells, editor layer cells, layer queries, effective opacity.
- `Maps.fs` - mutable runtime `TileMap`, tileset registry, tile/layer mutation, occupancy, walkability, effective opacity cache, exploration flags.
- `MapEditor.fs` - immutable `EditorTileMap`, resize, batch updates, layer editing, entity migration, runtime/editor conversion, `EditorHistory`.
- `PersistentVector.fs` - local persistent vector and transient vector implementation used by the editor map; see `docs/Persistent-Vector.md`.
- `FOV.fs` - rectangular field-of-view computation, translucency budget, visibility state, explored marking.
- `ChunkOcclusionManager.fs` - chunked opaque-cell tracking and occluder rectangle generation.
- `GameState.fs` - game model, doors, look-at results, interaction dispatch, visibility recomputation; see `docs/Interactions.md`.
- `Player.fs` - grid player movement, facing/pose updates, visual sprite synchronization.
- `Objects.fs` - current movable fixture helper code.
- `TilePropertiesSerializer.fs` - FlatBuffers serialization for tileset/tile properties.
- `MapTypeSerializer.fs` - FlatBuffers serialization for `TileMap`.
- `EntityRegistrySerializer.fs` - FlatBuffers serialization for entity registry data.
- `Localization.fs` - `.agl` parser, binary packer, and runtime localizer.
- `SpellsCore.fs` - engine-agnostic spell primitives and current mage-light casting boundary.

## Pending Documentation Queue

- None.

## Test And Script Map

- `tests.fsx` - broad engine tests.
- `map-tests.fsx` - map serialization/deserialization tests.
- `entity-registry-test.fsx` - entity registry serialization/deserialization tests.
- `localization-tests.fsx` - focused localization regression tests.
- `occluder-tests.fsx` - focused `ChunkOcclusionManager` rectangle, door, chunk-boundary, layer-opacity, clear, and view-filter tests.
- `persistent-vector-tests.fsx` - dedicated persistent vector correctness, one-shot sequence, and randomized model tests.
- `persistent-vector-bench.fsx` - deterministic local persistent vector speed benchmark.
- `fov-tests.fsx` - FOV/translucency/exploration characterization tests.
- `fov-oracle.fsx` - dense sampled FOV oracle for hard witness checks and soft visibility triage.
- `fov-bench.fsx` - visibility benchmark/experiment script.
- `fov-bench*.txt` - captured benchmark outputs.

## FlatBuffers Map

- `FlatBufferTypes/Schemas` - FlatBuffers schemas.
- `FlatBufferTypes/Generated` - generated C# FlatBuffers types.
- `FlatBufferTypes/AspectGameEngine.FlatBufferTypes.csproj` - generated-types project.

## Working Notes And Archives

- `sprite-system-redesign.md` - working note for sprite reference redesign.
- `chunkoccluder-inset.txt` - archived alternate occluder edge/inset approach.
- `changes.txt` - historical diff/log style notes.
- `archive/` - older FOV alternatives and experiments.

## External Frontend Notes

- `C:\Users\cybernetic\source\repos\aspectrpg\Design-docs\todo-list.md` - frontend todo list, including Godot-side lighting/shadow-shape work.

## Todo Sources

- `docs/triage.md` - active engine slice triage.
- `docs/todo.md` - active engine task checklist.
- `docs/Occluder-Task.md` - chunk occluder behavior, completed focused test coverage, and staged future work.
- Future task docs should be added under `docs/` when they become active technical work.

