# Map Format Versioning And Migrations

This document defines how authored maps survive engine data-model changes. Map compatibility is
an engine responsibility: old maps must either load with preserved meaning or fail with a precise,
actionable error. They must never become subtly different maps because a field was renamed,
reinterpreted, or removed without migration.

The immediate reason for establishing this system is `Tile.IsOccupied`. The current layered map
model already has terrain walkability, actors, and fixtures, so the old per-tile occupancy flag is
architecturally ambiguous. It cannot simply be deleted, however, because old serialized maps may
use `IsOccupied = true` as an anonymous per-cell movement block. A versioned migration must
preserve that behavior before the runtime shape can be cleaned up.

## Current State

Map serialization currently uses `FlatBufferTypes/Schemas/map_defs.fbs` and
`TileMapSerializer` in `MapTypeSerializer.fs`.

The current format has no map-format version. `TileMapFBS` contains dimensions, tiles, layer
cells, map metadata, spawn points, and explored flags. `TileFBS` stores `sprite_loc`, `health`,
and `is_occupied`. `TileMapSerializer.deserialize` reads those fields directly into the runtime
`TileMap` and `Tile` types.

Therefore every map written before this migration system exists is implicitly **format version
0**. The FlatBuffers library version embedded in generated code is not a map-format version and
must not be used as one.

Some compatibility handling already exists, such as defaulting missing spawn points and explored
data and falling back from the decals vector to the legacy single-decal field. Those are useful
precedents, but they are field-specific fallbacks rather than a general migration workflow.

## Ownership

`AspectGameEngine` owns:

- the current map-format version;
- parsing old map bytes;
- sequential semantic migrations;
- conversion from the current serialized document into runtime `TileMap` state;
- a byte-to-byte upgrade API and migration report;
- golden legacy fixtures and migration tests.

The Godot editor may own an explicit Upgrade Map command, file picker, progress display, and human-
readable report. It must call the engine migration API rather than reproduce migration semantics in
C#.

Normal runtime or editor loading may migrate a map **in memory**, but loading must never silently
rewrite the source file.

## Format Version Contract

Add `format_version:uint = 0` at the end of `TileMapFBS`. Existing files naturally read the
FlatBuffers scalar default of `0`; new files write the engine's explicit current version.

Version numbers describe the semantic map document, not releases, save-game versions, engine
assembly versions, or FlatBuffers package versions.

Schema evolution follows these rules:

1. Add new FlatBuffers fields only at the end of their table.
2. Never reorder existing fields.
3. Never reuse an obsolete field's slot for a new meaning.
4. Keep obsolete fields in the schema and mark them as legacy/deprecated in comments.
5. Give additive fields defaults that preserve old-file interpretation.
6. Regenerate C# with the repository's `flatc` tool; never edit generated C# manually.
7. Reject a map whose `format_version` is newer than the engine supports. Do not guess.

Every semantic change increments the current version deliberately, even when FlatBuffers could
technically supply a scalar default. The version says which interpretation is authoritative.

## Load Pipeline

Loading should become an explicit pipeline:

```text
bytes
  -> read serialized document and source version
  -> migrate vN to vN+1
  -> repeat until current version
  -> construct current runtime TileMap
  -> return TileMap plus MigrationReport
```

Each migration step is deterministic, operates on an in-memory serialized-document shape, and
advances exactly one version. A migration must not consult editor UI, rendering state, global
scene state, or the filesystem.

The engine should expose a rich load entry point such as `deserializeWithReport` that returns the
map, source version, final version, and applied migration ids. The existing `deserialize` API may
remain as a compatibility wrapper, but it must run through the same pipeline.

Current-version maps skip migration. Older supported maps run every missing step in order. Newer
maps fail before runtime state is constructed.

## Explicit Disk Upgrade

The engine should expose a byte-level operation such as:

```text
upgradeBytes(oldBytes) -> newBytes + MigrationReport
```

The default batch/editor workflow writes upgraded bytes to a separate destination. An explicitly
selected in-place upgrade must be transactional:

1. Read the complete original file.
2. Migrate in memory.
3. Serialize the current document to a sibling temporary file.
4. Reopen that temporary file through the current load pipeline.
5. Prove the required semantic invariants and expected target version.
6. Atomically replace the original only after every prior step succeeds.

The original remains untouched until the final replacement, making a partially written or
partially migrated map unreachable. Migration failure returns an error and report; it does not
leave a best-effort output behind.

A batch report should identify the source path, source version, target version, migrations
applied, and whether the map was unchanged, upgraded, skipped, or failed.

## Version 0 To Version 1: `IsOccupied`

The current movement model has three independent blocking inputs:

- `TileProperties.Walkable` for terrain-type walkability;
- actor and fixture occupancy in `LayerCell`;
- the legacy per-cell `Tile.IsOccupied` flag.

`Tile.IsOccupied` has no occupant identity or lifecycle. Nevertheless, version-0 maps may rely on
it. Version 1 should replace its runtime meaning with an explicit per-cell movement override:

```text
MovementBlockOverride.Inherit
MovementBlockOverride.Blocked
```

The version-0 migration is exact:

```text
is_occupied = false  -> MovementBlockOverride.Inherit
is_occupied = true   -> MovementBlockOverride.Blocked
```

Do not migrate `is_occupied = true` into `TileProperties.Walkable = false`. Tile properties are
shared by sprite/tile definition, while the old flag is stored independently on each map cell.
Changing shared tile properties could block every use of that tile art instead of only the legacy
cell.

Add the new override field at the end of `TileFBS`. Keep `is_occupied` in its original schema slot,
mark it legacy, and never reuse it. Version-1 serialization writes the new override and writes the
legacy field's neutral value. Version-1 loading reads only the new override; version-0 loading
reads `is_occupied` and migrates it before constructing the runtime tile.

After every map-loading path runs through this migration pipeline, the runtime/editor `Tile` shape
must drop `IsOccupied`. The field is legacy compatibility data, not part of the desired current
model. Until the migration path exists, removal is forbidden because it would make some old
blocked cells traversable.

The intended completion order is:

1. Commit real version-0 fixture bytes before changing the schema or writer.
2. Add format versioning, the serialized-document migration pipeline, and the version-0 reader.
3. Add and verify `MovementBlockOverride` as the current per-cell representation.
4. Prove version-0 passability and full map-state parity through migration tests.
5. Remove `IsOccupied` from runtime `Tile`, editor tile conversion/authoring, and all current-format
   movement logic.
6. Run the explicit upgrade tool over every authored project map, inspect the migration report, and
   replace the project maps with current-version outputs.
7. Retain the old `is_occupied` FlatBuffers slot, version-0 migration, and golden fixture so an
   untouched old map can still be upgraded later.

Completion means current source and authored current-version maps no longer use `IsOccupied`.
References may remain only in the legacy schema accessor, version-0 reader/migration, golden
fixtures, and compatibility tests. The map upgrade is part of the work; merely teaching the engine
to load old maps does not finish the cleanup.

The existing `PlayerActorNotAtPosition` result for a destination blocked only by the legacy flag
is misleading. Version 1 should classify the explicit override with a destination-side blocked
cause such as `DestinationBlockedByTileState`. That result change must receive focused coverage.

## Test Oracles

Migration tests must use committed binary fixtures produced by the old writer, not fixtures
regenerated by the current serializer during the test. At minimum, retain a real version-0 map
containing:

- a walkable cell with `IsOccupied = true`;
- a walkable cell with `IsOccupied = false`;
- a non-walkable tile;
- actor, fixture, item, and decal layer data;
- spawn points and explored data.

The test suite must prove:

- every version-0 cell has identical passability after version-1 migration;
- `IsOccupied = true` becomes the explicit blocked override;
- `IsOccupied = false` becomes inherit/default behavior;
- actors, fixtures, items, decals, health, sprite locations, spawn points, explored flags, map
  metadata, and dimensions survive unchanged;
- the migrated bytes load as the current version without applying the migration again;
- a current-version round trip preserves the override;
- a future unknown version is rejected;
- a failed disk upgrade leaves the original bytes untouched.

Every later migration needs its own golden source-version fixture and focused semantic oracles.
Broad round-trip tests remain useful but cannot replace old-writer fixtures.

## Design Boundaries

Map format migration and save-game migration are related but distinct. Authored map migrations
upgrade content structure. Save migration upgrades live campaign state and must account for the
authored-content version it overlays. The Stage 4 preflight establishes authored map versioning;
Stage 8 may extend the same principles to the live-state overlay.

Migration code preserves meaning; it does not silently repair unrelated bad content. If a legacy
state is ambiguous, the migration must either use a documented conservative mapping or fail with
an actionable error.

Do not make runtime gameplay branches support every historical representation forever. Historical
formats terminate at the migration boundary; runtime code sees only the current model.

## Limitations

The version field, serialized-document migration layer, version-0 fixture, upgrade API, and editor
command do not exist yet. All current maps remain implicit version 0.

The exact storage shape for `MovementBlockOverride` still needs to be added to the FlatBuffers
schema and runtime/editor tile models. Its two-state semantic contract is defined here; field and
type naming may follow established local conventions during implementation.

No support horizon has been chosen for very old map versions. Until one is deliberately adopted,
committed migrations and golden fixtures should be retained.

## Future Work

Extend the pipeline for Stage 4 item/container state and every later authored map schema change.

Add the frontend editor command after the engine byte-upgrade API is complete.

Define save-game format versioning and migrations alongside the Stage 8 live-state overlay.

Consider a standalone batch command for upgrading all maps in a content tree with a dry-run report
and separate-output default.

## Source Map

- `FlatBufferTypes/Schemas/map_defs.fbs` - current unversioned map and tile schema.
- `FlatBufferTypes/Tools/flatc.exe` - schema code generator.
- `MapTypeSerializer.fs` - current direct map serializer/deserializer and future migration entry
  point.
- `Maps.fs` - runtime `Tile`, `TileMap`, walkability, occupancy, and layer state.
- `MapEditor.fs` - editor/runtime tile conversion that must use the current tile model.
- `map-tests.fsx` - current map serialization and editor-conversion tests; future migration oracles.
- `docs/todo.md` - staged implementation checklist.
- `docs/triage.md` - current priority and stage coordination.

## Historical Notes

### 2026-07-10

- Added the first map-format versioning and migration design.
- Recorded all existing maps as implicit version 0 because `TileMapFBS` has no format field.
- Made migration an engine-owned Stage 4 preflight before new persisted inventory/container state.
- Defined append-only FlatBuffers evolution, sequential semantic migrations, explicit disk upgrades,
  transactional in-place replacement, and golden old-writer fixtures.
- Defined the first migration: preserve legacy `Tile.IsOccupied` behavior as an explicit per-cell
  movement-block override instead of deleting it or changing shared tile properties.
- Clarified the required end state: remove `IsOccupied` from current runtime/editor models and
  authoring after compatibility is proven, then explicitly upgrade every authored project map;
  retain the old FlatBuffers slot only for future legacy-file migration.
