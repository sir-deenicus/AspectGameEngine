# Animation And Actor Visual State

This document explains the current animation-facing contract in AspectGameEngine: actor frame references, player visual state, sprite selection, and serialization. The implementation currently lives in `Types.fs`, `LayerGrid.fs`, `GameState.fs`, `Player.fs`, `EntityRegistrySerializer.fs`, and `FlatBufferTypes/Schemas/entity_registry.fbs`.

The short version: animation is currently a frontend-neutral data and state contract, not a time-driven animation player. The engine stores actor frame references and picks the active `SpriteRef` for simple facing/pose changes. The frontend remains free to render those references as atlas cells, texture regions, scenes, spritesheets, shader-driven animation, or a richer visual system. That separation is intentional: gameplay/map state can stay stable while visuals improve independently.

## Explanation

The engine's current animation model has three pieces.

First, actor definitions can carry `NpcFrames`. `NpcFrames` is a compact four-slot frame set:

- `NormalLeft`
- `NormalRight`
- `AttackLeft`
- `AttackRight`

Each slot is a `SpriteRef`, so the frame can be a sheet cell, sheet region, cell span, irregular cell list, texture id, or scene path. The animation contract does not require all actor visuals to come from one kind of asset.

Second, runtime player state carries `PlayerVisualState`. This is mutable state with:

- `Facing: ActorFacing`
- `State: ActorPose`

`ActorFacing` is currently `Left` or `Right`. `ActorPose` is currently `Normal` or `Attack`.

Third, the player helper resolves the current visual state to one `SpriteRef` and writes it back into the player's registered actor `SpriteProperties`. That makes the active sprite visible through the same entity registry path used by the rest of the layer/rendering data.

## Current Sprite Selection Rule

The active player sprite is selected by this rule:

```text
Attack + Right -> AttackRight
Attack + Left  -> AttackLeft
Normal + Right -> NormalRight
Normal + Left  -> NormalLeft
```

Both `GameState.fs` and `Player.fs` currently contain a `resolvePlayerSprite` helper with this rule. `GameUpdate.createWithPlayer` registers the player actor using the initial visual state, and `Player.syncVisualToRender` updates the registered sprite after visual state changes.

`Player.setFacing` changes facing and syncs the registered sprite. `Player.setPose` changes pose and syncs the registered sprite.

`Player.tryMove` updates facing after successful horizontal movement. Moving right sets facing to `Right`; moving left sets facing to `Left`; vertical movement preserves the existing facing. Successful movement also syncs the visual and recomputes visibility.

## Actor State Versus Actor Pose

The current types distinguish `ActorState` from `ActorPose`.

`ActorState` has broader gameplay labels:

- `Idle`
- `Walking`
- `Attacking`
- `Spellcasting`
- `Normal`

`PlayerModel.PlayerState` stores an `ActorState`, but current sprite selection does not read it.

`ActorPose` is the smaller visual selector used by `PlayerVisualState`. It currently has only:

- `Normal`
- `Attack`

That means the live visual path is pose-driven, not full gameplay-state-driven. If walking, spellcasting, hit reactions, idle variants, or frame timing become engine-owned behavior, the project should explicitly connect or replace this split rather than quietly overloading one enum.

## SpriteRef Keeps Animation Frontend-Neutral

`NpcFrames` points to `SpriteRef` values rather than concrete renderer objects. This keeps animation data portable across visual implementations.

The same frame set can mix reference kinds:

- `SheetCell` for a single atlas-grid frame.
- `SheetRegion` for a cropped frame.
- `SheetSpan` for a rectangular multi-cell visual.
- `SheetCells` for irregular multi-cell visuals.
- `TextureId` for externally managed textures.
- `Scene` for a packed scene or prefab path.

This is useful for migration. A simple actor can begin as sheet cells, while a later frontend can replace one pose with a scene, a larger region, or another asset style without changing map placement or layer ownership.

## Player Registration Flow

`GameUpdate.createWithPlayer` is the current player setup path. It accepts:

- the initial `TileMap`
- `NpcFrames`
- a player actor id

It creates a starting `PlayerVisualState` with left-facing normal pose, writes a `SpriteType.Actor` entry into `EntityRegistry.SpriteProps`, places the actor id on the map at the selected spawn point, and creates the `GameModel`.

The registered player actor currently gets:

- `TileOpacity.Transparent`
- `DescKey = "player"`
- `NpcFrames = Some frames`
- `RenderLayer = 100`

After creation, player visual updates mutate the existing registry entry by replacing only its active `Sprite` field with the resolved `SpriteRef`.

This approach keeps layer cells stable. The map stores the actor id; the registry decides what that actor currently looks like.

## Serialization

Animation-facing data is serialized through the entity registry stream, not the map stream.

`entity_registry.fbs` stores optional `npc_frames` on `ActorPropertiesFBS`. Each of the four `NpcFramesFBS` slots is a `SpriteRefFBS` union, so frame references can use the same sprite reference forms as ordinary actor sprites.

`EntityRegistrySerializer.serializeFrom` writes actor `NpcFrames` when present. `EntityRegistrySerializer.deserialize` reconstructs `NpcFrames` only when all four frame slots can be read successfully. Actors without frame data deserialize with `NpcFrames = None`.

`entity-registry-test.fsx` currently verifies that actor frame slots round-trip across multiple `SpriteRef` variants, including sheet cells, sheet regions, texture ids, and scene paths.

Runtime `PlayerVisualState`, `PlayerState`, active pose, current facing, and animation timing are not serialized by `TileMapSerializer`. The map stores actor placement by id. The entity registry stores the actor visual definitions.

## Relationship To Maps And Layers

Animation does not change map placement. A player or actor remains an id in a runtime `LayerCell.ActorId`; changing visual state updates the registry entry for that id.

Animation does not change base tile data. It can indirectly affect visibility or occupancy only if an actor's registered properties change opacity or semantic type. The current player sprite sync preserves the actor properties and changes only the `Sprite` reference.

Animation does not change editor map structure. Editor maps place actor ids, fixtures, items, and decals. They do not currently store per-actor animation state.

## Design Boundaries And Operating Rules

The engine does not own frame timers yet. That is a useful boundary: renderers can use native animation players, shader animation, spritesheet playback, or scene-level animation without forcing the core map engine to understand frame clocks.

The engine does own enough visual state to answer simple gameplay-driven sprite selection. Facing and attack/normal pose can be changed from engine code, and the active sprite reference will stay consistent with the entity registry.

Do not hide registry mutation in constructors. Player registration is explicit in `GameUpdate.createWithPlayer`, and visual sync is explicit in `Player.syncVisualToRender`, `setFacing`, `setPose`, and movement.

Do not make map serialization responsible for active animation playback state unless the project chooses to persist runtime actor state as a broader save-game feature. Map files should stay content/map persistence, not accidental runtime snapshot files.

If animation grows beyond the four-slot `NpcFrames` model, preserve the frontend-neutral `SpriteRef` idea. A richer model should describe states, transitions, frame timing, and asset references without binding the engine to one renderer.

If `ActorState` becomes the source of truth for animation, document the mapping from gameplay state to visual state and test it. Today, `ActorState` exists but the sprite resolver uses `ActorPose`.

If animation frames change a sprite's lighting-shadow silhouette, keep that as sprite-shadow metadata attached to the visual frame or cycle. The design boundary for sprite lighting shadows is in `docs/Sprite-Shadows.md`.

## Future Work

The next animation step should be framed animation cycles that cover every visual category: base tiles, NPCs/actors, fixtures, items, decals, and any future layer sprite type. The goal is one shared data shape that stays engine-native, serializes cleanly, and can be handed to Godot without bespoke glue for every sprite kind.

The core shape should be a named cycle made of timed `SpriteRef` frames:

```fsharp
type AnimationLoop =
    | Once = 0
    | Loop = 1
    | PingPong = 2

type AnimationFrame =
    { Sprite: SpriteRef
      DurationMs: int
      OffsetX: int
      OffsetY: int }

type AnimationCycle =
    { Key: string
      Frames: AnimationFrame[]
      Loop: AnimationLoop
      FallbackSprite: SpriteRef option }
```

`Key` should be stable content vocabulary, not renderer vocabulary. Examples: `"idle-left"`, `"idle-right"`, `"walk-left"`, `"attack-right"`, `"water"`, `"torch-burning"`, `"item-sparkle"`, `"decal-glow"`.

`AnimationFrame.Sprite` should stay a `SpriteRef`. That lets a cycle mix sheet cells, sheet regions, spans, irregular cells, textures, and scenes just like the current static sprite path. `OffsetX` and `OffsetY` give the frontend a stable per-frame draw offset for taller sprites, bobbing items, fixture overlays, and tile effects without changing map coordinates.

The Godot handoff should be straightforward:

- `AnimationCycle.Key` maps to a Godot animation name.
- `AnimationCycle.Loop` maps to the Godot loop mode.
- `AnimationFrame.Sprite` resolves to the texture, atlas region, scene, or prefab reference.
- `AnimationFrame.DurationMs` maps to frame duration or to an animation speed plus per-frame duration.
- `AnimationFrame.OffsetX/Y` maps to sprite offset, node transform, or a generated wrapper node.

For base tiles, animation should live in tile properties, not in every map cell. A water tile, lava tile, animated grass tile, or machinery tile should point to an animation cycle through tileset data. The map cell should keep its stable `SpriteLoc`; the renderer should choose the displayed frame from the tile's animation cycle. Per-cell phase offsets can be added later if repeated animated tiles look too synchronized, but that should be explicit content data or deterministic from position, not hidden mutation of `Tile.SpriteLoc`.

For NPCs and actors, framed cycles should replace or sit beside `NpcFrames`. A future actor definition should be able to map visual state to cycle key:

```text
Idle + Left       -> "idle-left"
Idle + Right      -> "idle-right"
Walking + Left    -> "walk-left"
Walking + Right   -> "walk-right"
Attacking + Left  -> "attack-left"
Attacking + Right -> "attack-right"
Spellcasting + *  -> "cast-*"
```

This is the clean place to connect `ActorState` to visuals. `ActorPose` can either remain a small compatibility selector or be retired once a state/facing-to-cycle resolver exists.

For layer sprite types, the animation data should live with `SpriteProperties` or type-specific properties rather than with placement. Fixtures can have cycles such as `"idle"`, `"open"`, `"closed"`, `"lit"`, or `"broken"`. Items can have `"idle"` or `"sparkle"`. Decals can have `"idle"`, `"pulse"`, or `"fade"`. The runtime layer cell should still store ids; the registry should describe how those ids animate.

Do not make the map update loop rewrite sprite ids or tile sprite locations every frame. Animation playback should be a presentation decision computed from a cycle, elapsed time, and optional phase data. This keeps map editing, serialization, FOV, occlusion, and occupancy independent from visual timing.

Serialization should extend the existing asset streams deliberately. Tile animation cycles belong with tileset serialization. Actor, fixture, item, and decal cycles belong with entity registry serialization. Runtime playback state should not go into ordinary map files unless the project adds a broader save-game format.

Tests for this future work should pin the contract before any renderer integration:

- cycles round-trip through the appropriate serializer
- frame order and durations are preserved
- `SpriteRef` variants round-trip inside animation frames
- actor state/facing resolves to the expected cycle key
- tile animation data does not mutate `Tile.SpriteLoc`
- editor/runtime conversion preserves animated entity placement by id

## Source Map

- `Types.fs` - `NpcFrames`, `ActorFacing`, `ActorPose`, `ActorState`, and `PlayerVisualState`.
- `LayerGrid.fs` - `ActorProperties`, optional `NpcFrames`, `SpriteProperties`, and the entity registry.
- `GameState.fs` - player model creation, player actor registration, initial visual state, and one copy of current sprite resolution.
- `Player.fs` - player visual synchronization, movement-facing updates, `setFacing`, and `setPose`.
- `EntityRegistrySerializer.fs` - serialization and deserialization of actor `NpcFrames`.
- `FlatBufferTypes/Schemas/entity_registry.fbs` - `NpcFramesFBS` and actor registry schema.
- `entity-registry-test.fsx` - round-trip coverage for actor `NpcFrames` and mixed `SpriteRef` variants.

## Historical Notes

### 2026-07-05

- Added the first live animation document.
- Captured the current contract: animation is actor visual-state selection over `NpcFrames`, not a time-driven animation player.
- Recorded the current player flow: movement and explicit pose/facing changes sync the player's active `SpriteRef` into the entity registry.
- Documented the `ActorState` versus `ActorPose` split so future animation work can connect them intentionally.
- Recorded that animation definitions serialize through the entity registry stream, while maps store actor placement by id.
- Added future-work direction for framed animation cycles shared by tiles, NPCs/actors, fixtures, items, decals, and future layer sprite types, with a data shape designed to hand off cleanly to Godot.
