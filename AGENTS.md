# Agent Notes

This repository is an F# `net8.0` library for a tile-based game and editor. It is mostly data structures, map editing, serialization, visibility, occlusion, and small gameplay helpers. Treat it as engine code, not as a renderer or full game project.

## Start Here

Read these before substantial work:

- `PROJECT_INDEX.md` - orientation, docs, source map, and current task.
- `PROJECT_ETHOS.md` - project rules and engineering priorities--Always refresh on this on each fresh context or post-compaction

For current behavior, prefer the source over the old README. The README is useful but is scheduled for cleanup.

## Working Rules
- *NEVER* rush into code. Always plan and discuss before touching the codebase on each fresh context or post-compaction. Only start on receiving a go-ahead.
- Keep changes small and anchored to the current task.
- Preserve the runtime/editor split: `TileMap` is mutable runtime state, `EditorTileMap` is immutable editor state. 
- Do not touch global registries from constructors unless the project explicitly decides to make that ownership change.
- Keep serialization changes backward-aware. The FlatBuffers generated C# lives under `FlatBufferTypes/Generated`.
- Do not manually edit the Generated Flatbuffer C# code!
- Add or update focused `.fsx` tests when changing map semantics, layer semantics, visibility, occlusion, serialization, or editor conversion.
- Keep docs current when a system rule or known limitation changes.

## Build And Test

Build:

```powershell
dotnet build
```

Useful scripts:

```powershell
dotnet fsi .\tests.fsx
dotnet fsi .\map-tests.fsx
dotnet fsi .\entity-registry-test.fsx
```

`AspectGameEngine.fsproj` has a post-build copy step into `..\aspectrpg\Scripts\GameEngine\`. If that destination is missing, the build can fail even when the library code is fine.

## Documentation Habit

Root docs are for orientation and project-wide principles.

Technical docs and active task notes live in `docs/`.

Do not create design-doc folders here unless the project direction changes. This repo should stay focused on technical engine behavior.  

Put orientation and source maps in `PROJECT_INDEX.md`. 

When a technical task makes a choice between correctness, performance, and simplicity, record the choice in the relevant doc.

## Runtime And Editor Split

Runtime state should be cheap to mutate. `TileMap`, `LayerCell`, exploration flags, and opacity caches are runtime-facing structures.

Editor state should be cheap to snapshot. `EditorTileMap`, `EditorLayerCell`, `EditorHistory`, and `PersistentVector` are editor-facing structures.

Do not blur this split casually. If a feature needs both modes, name the conversion rule and test it both ways.

## Ownership 

Constructors should avoid hidden global lookups when load order can vary. Explicit initialization is better than a constructor that works only after some registry has been populated. 