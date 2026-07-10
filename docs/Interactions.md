# Interactions, Containers, Triggers, And Moveables

This document explains the current interaction surface and the intended engine-side direction for doors, containers, keys, triggers, levers, portcullises, signs, stairs, moveable fixtures, and other map objects.

The short version: interactions are gameplay/data behavior, not rendering behavior. An interactable may be represented by a base tile, a fixture sprite, an item, a decal, an actor, or a future layer type. The interaction system should target map objects through a storage-agnostic target shape, mutate map-local state, and return small result/message keys for the game/frontend to present.

## Current Behavior

The current live system has one real interaction behavior: doors.

`GameState.fs` defines `InteractionType`, `DoorAction`, `InteractionResult`, the legacy `InteractResult`, `GameModel`, `Doors.tryInteractDoorResult`, `GameUpdate.interactAtResult`, `GameUpdate.interactAutoAtResult`, and `GameUpdate.tryForInteractionsResult`.

`tryForInteractions` checks nearby candidate tiles around the player, looks for base tile properties with `Interactable = true`, then dispatches through `interactAutoAt`. The current scan order is facing tile, current tile, north tile, south tile, then behind tile. In frontend terms, this is the engine-side support for pressing the interact key, such as `E`, near an interactable one tile away. Today that auto path only recognizes `TileType.Door`, and it only sees base tile interactability.

Door open/close is visual-state driven. `Doors.tryInteractDoorResult` reads the current tile's `TileProperties.Visuals`, takes the first visual entry as the target state, updates the tile's `SpriteLoc`, and returns the visual key plus an `EngineChangeSet` in an `InteractionResult`. The older `Doors.tryInteractDoor`, `GameUpdate.interactAt`, `GameUpdate.interactAutoAt`, and `GameUpdate.tryForInteractions` wrappers still return the legacy `InteractResult` shape for existing callers.

Door auto-open during movement is structural, not localization-driven. `Doors.tryAutoOpenDoor` now treats the first visual transition as an auto-open transition only when the current door state is not open-like and the target door state is open-like by opacity. It still respects the current tile-index locked state. Description keys such as `tile.door.open` describe content for presentation; they do not decide whether the door opens, closes, or stays put.

Door lock state is only partial. `DoorAction.LockDoor` and `DoorAction.UnLockDoor` exist, and `GameModel.TileComplexStateInstance` can remember `ComplexState.ClosedDoor { Locked = true }` by tile index. `Doors.tryAutoOpenDoor` respects that locked state. The explicit open/close interaction path does not yet enforce lock or unlock behavior.

The engine has several interaction-shaped fields without dispatch behavior yet:

- `TileType` includes `Lever`, `Stairs`, `Container`, and `Sign`.
- `TileProperties.Interactable` marks base tile properties.
- `FixtureProperties.Interactable` marks fixture definitions.
- `DecalProperties.Interactable` marks decal definitions.
- `ItemProperties` gives items a description key, but not pickup/use behavior.
- `ObjectMovement` contains fixture pushing helpers. Player movement now has its own typed moveable path in `Player.tryMove`.

## Target Model

Interactions should be storage-agnostic.

A door may be authored as a base tile. A chest should usually be a fixture. A magic rune may be a decal. A dropped key may be an item. A talk target may be an actor. The interaction dispatcher should not care whether the target came from the base tile array or a layer cell once a target has been resolved.

The system should distinguish four concepts:

- Definition: reusable data saying what something is and what behavior it supports.
- Placement: one occurrence of that thing on a specific map.
- State: mutable, map-local runtime facts for that occurrence.
- Effects: map-local changes caused by an interaction or trigger.

Definitions belong in tile properties or entity registry data. Placements belong in the map. State belongs to the live game/map state and, when saving runtime progress, to the save stream. Effects should target map-local placements or coordinates, not Godot nodes.

This separation prevents a common mistake: keying live state by a reusable sprite/entity definition id. If three chests use the same chest definition, opening one must not open all three. A robust implementation needs a map-local object identity or a carefully defined coordinate-and-slot key for each interactive instance.

## Interaction Targets

A future target shape should describe what was hit without caring how it is rendered.

A useful shape would be similar to:

```fsharp
type InteractionSlot =
    | BaseTile
    | Fixture
    | Actor
    | Item of itemIndex:int
    | Decal of decalIndex:int

type InteractionTarget =
    { Position: GridPos
      Slot: InteractionSlot
      EntityId: int option
      LocalObjectId: int option }
```

`Position` is always the map coordinate. `Slot` tells which map layer or base tile was resolved. `EntityId` points back to reusable entity registry data when the target is a layer sprite. `LocalObjectId` is the stronger future identity for map-local state.

The resolver should choose the best interactable target in a deterministic order. For example, a player pressing interact while facing a tile should consider topmost/layer-priority interactable objects on the facing tile first, then current tile or nearby fallback positions according to the game's targeting rule.

The intended `E` key behavior is one-tile-nearby interaction. Pressing interact should first try the tile the player is facing. If nothing valid is there, it may fall back to the player's current tile and nearby adjacent candidates according to a documented order. The first implementation can preserve the current order, but the final resolver should make the order explicit and apply it to both base tiles and layered sprites.

The existing `lookAt` function already does a related top-object scan by render layer and stable order. Interaction targeting can reuse that idea, but it should not blindly equal draw order forever. Sometimes an invisible trigger, a floor rune, or a blocking fixture should win over a decorative top decal.

## Map-Local State

The current `GameModel.TileComplexStateInstance` dictionary is the first map-local state hook. It is keyed by flat tile index and stores `ComplexState`, currently only closed door lock state.

That shape is useful but too narrow for the next systems:

- It only targets base tile positions.
- It cannot distinguish multiple layer objects in one cell.
- It does not give reusable fixture instances unique state.
- It does not yet serialize as a live game-state/save stream.

The next robust base should add a map-local interaction state layer. It can start small, but it should be designed around instance identity:

```fsharp
type DoorInstanceState =
    { IsOpen: bool
      IsLocked: bool
      LockId: int option }

type ContainerInstanceState =
    { IsOpen: bool
      IsEmptied: bool
      BaseWeight: int
      Contents: ContainerContents }

type MoveableInstanceState =
    { BaseRequiredStrength: int }

type InteractionInstanceState =
    | DoorState of DoorInstanceState
    | ContainerState of ContainerInstanceState
    | MoveableState of MoveableInstanceState
    | TriggerState of TriggerInstanceState
```

The exact type names can change, but the rule should hold: mutable facts about an occurrence on a map are map-local. They do not belong in the global entity registry, and they should not be inferred solely from the rendered sprite.

`LockId` identifies the lock, not a particular physical key placement. The door checks inventory for a key item definition whose `ItemKind.Key` payload opens that lock id. This follows the general item rule: shared presentation/default data does not define gameplay identity. Two items that share art and weight but differ in engine-owned behavior should usually be separate item definitions or capability payloads, not two unique placed instances of one definition. Keys are only the first concrete case.

Moveable containers should calculate effective movement cost from map-local state. A chest can have a base/default weight, but its live contents add to effective weight. Removing items from one placed chest can make that chest easier to move without changing the shared fixture definition or every other chest using the same art.

## Containers And Keys

The immediate gameplay slice is a container with a key that opens a locked door.

There is no visible inventory requirement for the first pass, but the engine state should still use the same item and inventory model that later visible inventory will use. A key-ring shortcut would be acceptable only as an internal query view over player inventory, not as the authoritative state.

The first slice should behave like this:

- A locked door has a lock id.
- Interacting with the locked door without the key returns a locked message and does not open the door.
- A container has contents that include an item definition whose key payload opens the matching lock id.
- Interacting with the container transfers the key item into player inventory and marks the container opened/emptied.
- Interacting with the locked door after taking the key unlocks and opens it, or unlocks it and lets a second interaction open it if the game wants that pacing.
- Re-interacting with the emptied container does not duplicate the key.

This should be real engine state, not a demo shortcut. The key should not be represented only as a frontend popup or a transient message. The door should not check a hardcoded coordinate. The container should not be keyed by shared sprite id.

## Triggers And Effects

Levers and more complex interactions need a small trigger/effect model.

A lever opening a portcullis is the basic example:

```text
interact lever A -> execute trigger A -> set portcullis B open
```

The engine should model this as data. The lever does not need to know how to mutate a specific renderer node. It should fire one or more engine effects against map-local targets.

A future effect list could include:

- set a door open or closed
- lock or unlock a door
- swap a tile or fixture visual state
- enable, disable, spawn, or remove a fixture
- move a map object
- grant or remove a key
- mark a container opened or emptied
- emit a game event key for higher-level game code

This keeps simple cases declarative and keeps complex cases extensible. If a later quest script needs to react, the interaction can emit an event key while still applying immediate map effects locally.

Triggers need stable targets. A lever should target a portcullis by local object id, explicit map coordinate plus slot, or a named map-local tag. Local object ids are the best long-term base; tags are useful for authoring, but duplicate tags need deterministic behavior.

## Authoring Ladder And Data Format

Interaction behavior should use a three-step ladder.

First choice: effect lists in data files. Most interactions should be authored as conditions and effects, then interpreted by the F# engine. This covers containers, keys, locks, doors, levers, portcullises, simple state changes, messages, and local trigger chains without compiling new engine code.

Second choice: F# behavior. If an interaction needs richer logic than the effect list can express, add a typed F# primitive or behavior handler. This keeps correctness, tests, performance, and save/load under the engine's control. F# behavior is compiled, so it is not the fastest iteration path, but it is the right place for rules that become part of the permanent engine.

Third choice: trusted Godot/GDScript fallback. If both data effects and F# behavior are insufficient, the frontend can catch an emitted event or run a trusted script hook. This is an escape hatch, not the foundation. GDScript from strings is not a security sandbox, so release builds should treat it as trusted code only and gate it behind manifest/version/checksum rules if used.

The runtime data format should be FlatBuffers, not JSON. This project is compiled with `--reflectionfree`, and the interaction system should not depend on runtime reflection or on a new hand-authored general parser. FlatBuffers already fits the project: schemas are explicit, generated accessors are reflection-free, binary data is compact, and the existing build already owns `flatc` and generated C# types.

That makes the split:

- authored source: editor UI, Godot tooling, or typed F# builder scripts
- interchange/runtime file: FlatBuffers binary
- engine reader: generated FlatBuffers accessors plus explicit F# mapping/validation

An interaction schema can still be versioned and declarative:

```fbs
namespace AspectGameEngine.FBS;

enum InteractionEffectKindFBS : byte {
  GrantKey = 1,
  SetDoorOpen = 2,
  SetDoorLocked = 3,
  SetContainerEmptied = 4,
  Message = 5,
  EmitEvent = 6
}

table InteractionTargetRefFBS {
  object_id:int;
  tag:string;
  x:int;
  y:int;
  slot:byte;
}

table InteractionEffectFBS {
  kind:InteractionEffectKindFBS;
  target:InteractionTargetRefFBS;
  key_id:int;
  bool_value:bool;
  text_key:string;
  event_key:string;
}

table InteractionRuleFBS {
  id:string;
  target:InteractionTargetRefFBS;
  effects:[InteractionEffectFBS];
}
```

The exact schema can change, but the format decision should hold: generated binary data at runtime, not reflection-based JSON.

For hand editing, do not invent a custom text language just to make interaction files pleasant. If plain text authoring becomes important, prefer a tool path that compiles into FlatBuffers before runtime, or use F# builder scripts during development to emit the binary data. The engine should load the binary contract and validate it, not parse a second source language.

## F# Builder Scripts

F# builder scripts are the first authoring surface. They are content-building scripts, not runtime gameplay scripts.

An authoring script should run during development, import, or build. It constructs typed interaction definitions through a small DSL and writes a FlatBuffers interaction bundle. The script itself is not shipped as the authoritative runtime behavior, and the engine does not evaluate it while the game is running.

This is the middle ground: `.fsx` gives authoring an ergonomic F# scripting surface, while `.interactions.bin` remains the runtime/game input. The shipped game consumes the same validated binary artifact regardless of whether it was produced by an F# script, an editor panel, or other build tooling.

A builder script can look like this:

```fsharp
interactionPack "intro_room" [
    container "entry_chest" [
        onInteract [
            grantKey 7
            setContainerEmptied self true
            message "container.key_taken"
        ]
    ]

    door "entry_door" [
        lockedWithKey 7
    ]

    lever "north_lever" [
        onInteract [
            setDoorOpen (tag "north_portcullis") true
            message "lever.portcullis_opened"
        ]
    ]
]
|> InteractionCompiler.write "intro_room.interactions.bin"
```

That gives content authors a pleasant F# surface without creating a new parser. The compiler side should still emit the same schema-backed binary that editor UI would emit later.

Builder scripts should be deterministic:

- given the same source and referenced map/object ids, emit the same bundle bytes or the same logical data
- fail on duplicate local ids, duplicate tags where uniqueness is required, unknown effect kinds, invalid key ids, and malformed targets
- produce diagnostics with object id, tag, and source helper name where possible
- write a schema/version number into the bundle

Editor-mode tooling may watch authoring inputs, rerun the builder, and reload the generated bundle for faster iteration. The exact reload policy is intentionally not locked yet: state reset, state preservation, validation failures, and playtest behavior should be designed once the first interaction bundle and editor workflow exist.

## Map Loading And Validation

Interaction bundles should be associated with maps, but they should not be hidden inside unrelated runtime state.

The first practical layout can be sidecar-based:

```text
intro_room.map.bin
intro_room.interactions.bin
```

Longer term, the map manifest or editor export can name the interaction bundle explicitly. Embedding interaction data directly into the map stream is also possible later, but sidecars keep the first implementation smaller and avoid churning the existing map serializer while the interaction contract is still settling.

The engine load flow should be:

```text
load TileMap
load interaction bundle
validate bundle against map and entity registry
build map-local interaction runtime state
create GameModel with map + interaction state
```

Validation should happen before play starts. The loader should check that referenced local object ids, tags, coordinates, slots, key ids, initial door/container states, and effect targets are coherent. If the bundle references an object that is not present in the map, that should be a load error or a clearly reported validation warning, not a silent no-op.

The interaction runtime state belongs beside `GameModel`, not in global registries. A loaded bundle supplies default interaction definitions and initial state. The live game state records facts such as acquired keys, opened containers, moved barrels, triggered levers, and changed door locks.

## Godot Frontend Consumption

The Godot layer should consume interactions by asking the engine to resolve and execute them.

For player input:

```text
player presses interact
Godot calls engine interaction entry point
engine resolves target from map/layers/player position
engine applies validated effects to GameModel/TileMap
engine returns result keys and changed-state hints
Godot updates visuals, messages, sounds, and UI
```

For movement into a moveable:

```text
player attempts movement
Godot calls engine movement entry point
engine detects moveable interaction
engine pushes or swaps according to movement rules
engine recomputes affected map state and visibility
Godot redraws from the changed map/layer state
```

The frontend should not duplicate interaction rules. It should not decide whether a key opens a door, whether a container is emptied, or whether a lever opens a portcullis. Those are engine decisions. The frontend should present the outcome: message keys, sounds, animation cues, changed sprites, light/shadow refresh, and optional event handling.

If an effect emits a frontend event key, Godot may handle it with trusted code:

```text
engine effect: EmitEvent "quest.intro.play_stinger"
Godot event router handles sound/camera/UI behavior
```

If a trusted GDScript fallback is ever used, it should receive a narrow context and return declarative effects or frontend-only presentation actions. It should not receive raw authority over `GameModel`, `TileMap`, global registries, or arbitrary node paths.

## Moveables

Moveables are interactables.

A fixture is moveable when its registered `FixtureProperties.Moveable` value is positive. That value is a movement requirement, not a boolean flag. Today the player strength used by `Player.tryMove` is effectively fixed at `1`, so `Moveable = 1` can move, `Moveable = 2+` is too heavy, and `Moveable = 0` is an ordinary non-moveable fixture. The contract should stay shaped as requirement versus actor/player strength so later stats, buffs, encumbrance, container contents, party members, or other strength sources can participate without changing map data.

For this game, moving into a moveable object should attempt to move that object in the same direction as the player. This applies to all eight movement directions.

The rule:

```text
player attempts move by delta into moveable object
if object position + delta is open:
    move object to object position + delta
    move player into object's old position
else:
    swap player and object positions
```

Example, with the player moving left:

```text
WBP -> WPB
```

`W` is a wall, `B` is the moveable object, and `P` is the player. The barrel cannot move left into the wall, so it swaps with the player instead.

For simultaneous or multi-PC turns, resolve PC movement first, then evaluate moveable push/swap against the resulting occupied cells. For example, if `@` is the active player and `%` is another PC/actor:

```text
#B@%
```

If both actors move left, `@` cannot push `B` left because `#` blocks the push destination. The swap fallback is also blocked if `%` has moved into `@`'s old cell during the same turn. In that case, the moveable interaction fails with the normal blocked result, such as `Cannot move [object name]: movement blocked.`

This should be part of movement resolution, not a separate button-only action. A move attempt into a moveable is an interaction attempt. The implementation should still validate bounds, occupancy, walkability, fixture movement rules, simultaneous movement occupancy, and effective opacity updates. After a successful move or swap, visibility should be recomputed just like normal player movement.

Before the Stage 1 draft, `Objects.fs` used a different fallback after the same-direction push. That behavior is not the desired rule. The intended rule is simpler and stricter: push if the forward destination is open, otherwise swap with the player.

## Movement Result Contract

Movement is moving from boolean-only APIs to typed result data.

`Types.fs` now owns the small shared vocabulary used by the movement slice:

- `EngineMessage` and `EngineMessageArg` for localization keys plus typed arguments.
- `EngineChangeSet` for changed base cells, changed layer cells, changed entities, visibility-impact flags, occlusion-impact flags, and save-relevant changes.
- `MovementBlockedCause` for deterministic failure reasons.
- `MovementKind` for normal movement, pushed moveables, and swapped moveables.
- `MovedMapObject` and `MovementResult` for player old/new position and moved object old/new position.

`Player.tryMove` is the typed movement entry point and returns `MovementResult`. The old boolean shape is now the explicit compatibility wrapper `Player.tryMoveBool`. Movement results include stable message keys for normal movement, moveable push/swap outcomes, and deterministic blocked causes.

Stage 3 keeps those message keys as keys. `EngineMessage.Args` remains a typed array of `EngineMessageArg` values, and `EngineMessageLocalization` converts those values to localization `Args` only at the presentation boundary. Runtime interaction and movement logic should branch on typed causes, map state, tile/entity metadata, lock state, and opacity state, never on rendered strings or description keys.

Stage 1 moveables are implemented and verified. Code routes movement into blocking moveable fixtures through `Player.tryMove`, uses `TileMap.TryPushFixtureAndMoveActor` for atomic push-plus-player movement, uses `TileMap.TrySwapActorAndFixture` for the swap case, and updates `Objects.fs` to the push-or-swap fallback. Focused movement tests cover push, swap, blocked cases, diagonal movement, opacity changes, and moved actor/fixture map serialization.

Stage 2 mutation hints are implemented for movement and current door interactions. `EngineChangeSet` reports changed base cells, changed layer cells, changed entities, visibility/FOV refresh needs, occlusion-input changes, and save relevance. Movement marks visibility changed on successful movement because the player/FOV origin moved. It marks occlusion changed only when effective opacity changes, such as an opaque moveable changing cells or a door tile changing opacity. Transparent actor or fixture movement still reports layer/entity changes without forcing occlusion rebuilds.

`GameUpdate.lookAt` is the current Stage 3 description scaffold. It returns the base tile description key plus up to three layer object description keys by render-layer/stable-order priority, using `TileMap.TryGetTileDescriptionKey` and `SpritePropsQueries.tryGetDescriptionKey`. Missing entity description keys are not returned as fake strings; callers can see that layer objects exist through `HasMore`.

Current door interactions use typed `InteractionResult` entry points:

- `Doors.tryInteractDoorResult`
- `GameUpdate.interactAtResult`
- `GameUpdate.interactAutoAtResult`
- `GameUpdate.tryForInteractionsResult`

The older message-only wrappers remain for compatibility. Later interaction features should return the typed result shape directly and keep wrapper APIs thin.

## Serialization

There are two persistence concerns.

Authored interaction definitions and defaults belong with existing content streams:

- base tile interaction defaults in tileset properties
- entity interaction defaults in entity registry data
- map placements in map serialization

Live interaction state belongs to runtime game/save state. The current map serializer writes `TileMap`, but there is no full `GameModel` save stream yet. Door locks, opened chests, emptied containers, trigger latches, moved barrels, player inventory, dropped or picked-up item stacks, and container contents are all live progress state.

The save stream should behave as a live-state overlay over authored content. It can be compact like a diff, but it should record the current truth directly rather than replaying every command: moved object positions, changed door open/lock facts, container inventories after transfers, acquired or dropped items, trigger states, and future tile-swap state.

Until a full save stream exists, tests can construct state directly. But the design should not hide live state in global registries or renderer data, because those choices would make save/load and map reuse brittle.

## Design Boundaries And Operating Rules

Interactions are engine/game data behavior. They are not renderer behavior and not UI behavior.

Interactability must be agnostic to whether the object is backed by a base tile or a layered sprite. The resolver can inspect both storage paths; the handler should receive a target and behavior/state data.

Build robust bases, not teardown demos. A narrow first feature is good only if it strengthens the permanent interaction model. Avoid one-off hardcoded coordinates, shared-definition instance state, or frontend-only progress flags.

Map-local mutable state must stay map-local. Reusable registry definitions can say "this kind of object is a container" or "this fixture is moveable"; they cannot remember that one particular chest in one particular map has already been opened.

The engine should return compact result keys and state changes. The Godot layer can choose sounds, popups, inventory UI, animations, and visual effects.

Triggers should target map-local objects, positions, slots, or tags. They should not target Godot node paths.

Moveables are interactions driven by movement. They should use the same collision, occupancy, map mutation, opacity-cache, and visibility update discipline as ordinary movement.

## Future Work

Add a real interaction target resolver that scans base tiles and layer cells, respects interactable metadata, and returns an `InteractionTarget`.

Make the interact-key target rule explicit in tests: pressing `E` or the frontend's equivalent interact command should resolve a valid interactable one tile away, preferring the facing tile before fallback candidates.

Add map-local instance identity for interactive placements. This is the cleanest base for containers, levers, moved fixtures, trigger targets, and save/load.

Replace the narrow `TileComplexStateInstance` dictionary with a broader map-local interaction state store, or fold it into one unified state store while preserving backward compatibility for existing door state.

Implement locked door correctness: open/close must respect lock state, unlock must require the matching held key, and auto-open must remain fast.

Implement the first container/key slice with no visible inventory UI but real inventory state: container transfers a key item, key definition opens a lock id, locked door checks inventory, and repeated container use does not duplicate the key.

Implement item definition defaults/templates so many items can share icon, sprite art, weight, value, and stack policy while remaining distinct item definitions when their names, descriptions, or engine-owned capability payloads differ. Key definitions and lock payloads are the first concrete use.

Implement moveable-container effective weight from base/default weight plus live contents.

Add generic map-local movement-hold state for moveable objects. This should block movement independent of weight: weight answers whether an actor is strong enough to push something, while hold state answers whether that object is allowed to move at all. The holding cause should be generic, such as a spell field, binding effect, latch, or other restraint that can later be disabled or dispelled.

Add trigger/effect data for lever-to-portcullis and similar local map changes.

Add a versioned FlatBuffers interaction data schema, loader, and explicit validation with clear error reporting.

Add an F# builder DSL/compiler for interaction authoring scripts that emits the same FlatBuffers bundle the editor will later emit.

Add map-to-interaction-bundle association, initially as a sidecar path or manifest entry.

Explore editor-mode rebuild/reload tooling for generated interaction bundles without making a specific live-reload policy part of the runtime contract yet.

Add a frontend-facing interaction execution API that returns message keys, event keys, and changed-state hints without making Godot duplicate engine rules.

Add an F# behavior registry for interactions that graduate beyond declarative effect lists.

Add a trusted Godot/GDScript event-hook fallback only after data effects and F# behavior prove insufficient.

Add serialization for authored interaction defaults first, then live map/game state as an overlay/diff save stream when the save stream exists.

Add focused tests for interaction targeting, locked doors, containers, key acquisition, trigger effects, and moveable push/swap edge cases.

## Source Map

- `Types.fs` - tile types, tile properties, `ComplexState`, and door lock state shape.
- `LayerGrid.fs` - fixture, item, actor, and decal properties; layer cells; interactable fixture/decal flags.
- `Maps.fs` - runtime map mutation, occupancy, fixture movement, actor/fixture swap, items, and layer cells.
- `MapEditor.fs` - editor placement of fixtures, items, decals, and actors.
- `GameState.fs` - current game model, door interactions, look-at, interaction dispatch, and visibility recomputation.
- `Player.fs` - typed player movement result API, facing, actor movement, moveable fixture path, and door auto-open call.
- `Objects.fs` - moveable fixture helper code now aligned with push-forward-else-swap.
- future `InteractionCompiler` - F# builder-script compiler that writes interaction FlatBuffers bundles.
- future interaction FlatBuffers schema - binary interaction definitions and effect lists.
- `TilePropertiesSerializer.fs` - current tileset serialization for `TileProperties.Interactable`, `TileType`, and door complex state.
- `EntityRegistrySerializer.fs` - current registry serialization for fixture/decal interactable flags and item definitions.
- `map-tests.fsx` - current door interaction tests and likely home for first interaction behavior tests.

## Historical Notes

### 2026-07-07

- Added the first shared movement result vocabulary in `Types.fs`: typed messages, change sets, moved-object identity, movement kinds, and deterministic blocked causes.
- Established `Player.tryMove` as the rich `MovementResult` API and `Player.tryMoveBool` as the explicit boolean compatibility wrapper.
- Began Stage 1 moveable integration: added a draft player movement path for blocking moveable fixtures, added `TileMap.TrySwapActorAndFixture`, and updated `Objects.fs` to use push-forward-else-swap instead of the earlier alternate fallback.
- Advanced Stage 1 movement to the verification boundary: added atomic push-plus-player movement with `TileMap.TryPushFixtureAndMoveActor`, added stable movement result message keys, and fixed/extended `movement-tests.fsx`.
- Completed Stage 1 verification: `movement-tests.fsx` and `tests.fsx` pass, including movement edge cases, opacity-changing moveables, and moved actor/fixture serialization coverage.
- Paused the implementation for compaction before movement tests were green; `docs/todo.md` records the recovery state and the first failing test-script syntax issue.
- Clarified item/lock/container state: doors store lock ids, key item definitions advertise opened lock ids, visually identical items with different engine-owned capability payloads are distinct definitions with shared defaults, and moveable containers derive effective weight from live contents.
- Clarified save-game direction as a live-state overlay over authored content, covering changed doors, moved objects, player inventory, container inventories, picked-up/dropped items, and other map-local progress state.
- Completed Stage 2 changed-state support: movement and current door interactions return typed change sets with changed cells/entities, visibility/FOV hints, occlusion hints, and save relevance; legacy interaction wrappers remain message-only compatibility APIs.
- Completed Stage 3 description/localization support: movement and interaction messages stay key-plus-typed-args, `lookAt` uses the shared base/entity description-key contract, and door auto-open no longer branches on description keys.
- Clarified moveable turn ordering: push-or-swap is intentional, and simultaneous multi-PC turns should resolve PC movement first, then evaluate push/swap against the resulting occupied cells.
- Added future-work direction for generic map-local movement-hold state that blocks object movement independently from weight or strength checks.

### 2026-07-05

- Added the first live interactions document.
- Recorded that doors are the only implemented interaction behavior today, while containers, levers, stairs, signs, fixture interactability, decal interactability, and item behavior are mostly metadata.
- Recorded the current interact-key support: `tryForInteractions` checks nearby candidates, with the facing tile first, so pressing `E` near an interactable one tile away is the intended engine-side use case.
- Set the design direction that interactables are storage-agnostic: base tiles and layered sprites should resolve into a common interaction target.
- Recorded the need for map-local interaction state so one opened chest, moved barrel, or triggered portcullis does not mutate a shared registry definition.
- Captured the first required gameplay slice: a container grants an invisible key that opens a matching locked door.
- Captured the moveable rule: movement into a moveable pushes it in the same direction when possible, otherwise swaps the player and moveable object, for all eight directions.
- Recorded the interaction authoring ladder: FlatBuffers effect-list data first, typed F# behavior second, trusted Godot/GDScript fallback only as an escape hatch.
- Clarified that F# builder scripts are compile/import-time authoring tools that emit FlatBuffers bundles, not runtime scripts.
- Clarified the middle-ground workflow: `.fsx` is authoring input, `.interactions.bin` is runtime input, and editor reload behavior remains an open tooling policy.
- Added the map consumption flow: load map, load interaction bundle, validate against map/registry, then build map-local runtime state.
- Added the Godot consumption flow: frontend sends input intent to the engine, then presents returned message/event keys and changed map state.
