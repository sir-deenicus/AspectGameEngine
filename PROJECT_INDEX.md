# Project Index

Start here when orienting yourself in AspectGameEngine.

## Agent Entry Points

- `AGENTS.md` - repository instructions for coding agents.
- `PROJECT_ETHOS.md` - project-wide engineering principles.
- `docs/Occluder-Task.md` - current first technical task.
- `Readme.md` - older overview; useful, but scheduled for cleanup.

## Current Task

The first task is occluder work:

- stabilize the current `ChunkOcclusionManager`
- decide what output contract it should expose
- add focused tests for wall rectangles, door behavior, dirty rebuilds, and view iteration
- use `chunkoccluder-inset.txt` as archived context for a possible edge/inset approach, not as the current implementation

Canonical task doc: `docs/Occluder-Task.md`.

## Documentation Layout

Root docs:

- `AGENTS.md`
- `PROJECT_ETHOS.md`
- `PROJECT_INDEX.md`

Technical docs and task notes:

- `docs/Occluder-Task.md`
- `docs/Fog-Of-War.md`

No design-doc tree exists here. Keep `docs/` technical unless the project direction changes.

## Code Map

- `AspectGameEngine.fsproj` - F# project file, compile order, FlatBuffers package, generated-types project reference, post-build copy.
- `Types.fs` - foundational grid, sprite, tile, opacity, biome, map type, and tile-property types.
- `LayerGrid.fs` - entity sprite properties, global entity registry, runtime layer cells, editor layer cells, layer queries, effective opacity.
- `Maps.fs` - mutable runtime `TileMap`, tileset registry, tile/layer mutation, occupancy, walkability, effective opacity cache, exploration flags.
- `MapEditor.fs` - immutable `EditorTileMap`, resize, batch updates, layer editing, entity migration, runtime/editor conversion, `EditorHistory`.
- `PersistentVector.fs` - local persistent vector and transient vector implementation used by the editor map.
- `FOV.fs` - rectangular field-of-view computation, translucency budget, visibility state, explored marking.
- `ChunkOcclusionManager.fs` - chunked opaque-cell tracking and occluder rectangle generation.
- `GameState.fs` - game model, doors, look-at results, interaction dispatch, visibility recomputation.
- `Player.fs` - grid player movement, facing/pose updates, visual sprite synchronization.
- `Objects.fs` - current movable fixture helper code.
- `TilePropertiesSerializer.fs` - FlatBuffers serialization for tileset/tile properties.
- `MapTypeSerializer.fs` - FlatBuffers serialization for `TileMap`.
- `EntityRegistrySerializer.fs` - FlatBuffers serialization for entity registry data.
- `Localization.fs` - `.agl` parser, binary packer, and runtime localizer.
- `SpellsCore.fs` - engine-agnostic spell primitives and current mage-light casting boundary.

## Test And Script Map

- `tests.fsx` - broad engine tests.
- `map-tests.fsx` - map serialization/deserialization tests.
- `entity-registry-test.fsx` - entity registry serialization/deserialization tests.
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

## Todo Sources

- `docs/Occluder-Task.md` - active first task.
- Future task docs should be added under `docs/` when they become active technical work.
