# Triage: Light, Lock, And Look Engine Slice

This triage note is for the active engine slice only. The long-running engine checklist lives in `docs/todo.md`.

## Slice Goal

Build the durable engine base for the frontend's Light, Lock, And Look slice. The stages below are intentionally lined up with the frontend stages: engine Stage 1 should unblock frontend Stage 1, engine Stage 2 should unblock frontend Stage 2, and so on.

The engine owns durable state, rules, localization keys, visibility/occlusion inputs, serialization, and tests. It does not own rendering, UI layout, input binding, editor panels, shaders, particles, lights, shadows, or visual effects.

## How To Mark Work

Use this notation in `docs/todo.md`:

```text
- [ ] [NOW] [T1] Work item.
```

Status tags:

- `[NOW]` - current sharp edge.
- `[TODO]` - planned for this engine slice.
- `[BLOCKED]` - cannot move without a decision or missing source.
- `[DONE]` - completed and verified.
- `[DEFERRED]` - intentionally moved out of this slice.

Tier tags use `model-tier-notes.md`. They mean minimum executor tier, not priority:

- `[T1]` - architecture, persistence, map mutation, public API, visibility, localization, or silent-failure-sensitive work.
- `[T2]` - implementation that is safe after a `[T1]` contract names decisions and verification gates.
- `[T3]` - mechanical follow-through with strict steps and checks.

## Coordination Rule

Stage numbers coordinate across repos. Frontend Stage N may depend on engine Stage 0 through engine Stage N. It should not depend on engine Stage N+1 or later.

If a frontend stage needs engine support, that support belongs in the matching engine stage, an earlier engine stage, or Stage 0 if it is a shared contract needed by multiple stages. If the support naturally belongs later, move the frontend work later too.

Engine Stage 7 should not block frontend Stage 2. If frontend rendering foundation needs generic invalidation hints, those hints belong in engine Stage 2. Stage 7 keeps only the lighting/day-night-specific durable data and visibility/occlusion details needed by the lighting stage.

## Current Focus

- `[NOW] [T1]` Stage 4 preflight: establish map format versioning, sequential migrations, golden version-0 fixtures, and transactional explicit upgrades per `docs/Map-Migrations.md`.
- `[TODO] [T1]` Stage 4 features: items, pickup, drop, hidden inventory, and containers after the preflight.

## Current State

Stages 0, 1, 2, and 3 are complete.

Current state:

- Movement contract types exist in `Types.fs`.
- `Player.tryMove` is the typed `MovementResult` API; `Player.tryMoveBool` is the boolean wrapper.
- `InteractionResult` is the typed interaction-result shape for current door interactions; `GameUpdate.interactAtResult`, `interactAutoAtResult`, and `tryForInteractionsResult` preserve changed-state data, while legacy wrappers remain message-only.
- Moveable push-or-swap code exists in `Player.fs`, with `TileMap.TryPushFixtureAndMoveActor` and `TileMap.TrySwapActorAndFixture` in `Maps.fs`.
- The 2026-07-10 LD54 hardening is complete: `Player.tryMove` uses one internal typed `TileMap` transaction for collision classification and normal/push/swap mutation, with scalar opacity tracking and no public prevalidated bypass. Rich accepted/wall-blocked movement now measures 552/0 allocated bytes per call versus the approximately 1,744/136-byte audit baseline; `tryMoveBool` measures 24/0. Stage 4 remains the current feature focus.
- Push-or-swap is intentional. For simultaneous multi-PC turns, PC movement should resolve first; moveable push/swap then validates against the resulting occupied cells.
- Moveable strength is intentionally a requirement-versus-strength contract: current player strength is effectively `1`, but future player stats, buffs, encumbrance, container contents, or party help should be able to affect whether a moveable fixture can move.
- `Objects.fs` has been updated away from the old push-forward-else-behind-player rule.
- Movement change sets report changed layer cells, changed actor/fixture entities, visibility refresh needs, save relevance, and occlusion changes only when effective opacity changes or a door tile changes.
- Door interaction results report changed base cells/entities, message keys, visibility/occlusion hints, and save relevance.
- Stage 2H visibility ownership is complete on the engine side: `GameModel.VisibilityState` is a derived cache, creation starts empty, movement/interactions/setters do not recompute eagerly, and consumers refresh through explicit `GameUpdate.recomputeVisibility`.
- `SpritePropsQueries.tryGetDescriptionKey` exposes description keys for actor, fixture, item, and decal definitions. `TileMap.TryGetTileDescriptionKey` remains the base tile description-key path.
- `EngineMessageLocalization` converts typed `EngineMessage` values to localization `Args` and formats them through a caller-owned `Localizer`.
- Door auto-open branches on typed tile/visual/opacity state instead of description keys.
- Current map bytes have no format-version field, so all existing maps are implicit version 0. `Tile.IsOccupied` remains a serialized per-cell movement block and may not be deleted until the version-0 migration preserves it as an explicit current-model override. The preflight then deletes it from current runtime/editor models and explicitly upgrades every authored project map; only legacy migration support remains.
- `movement-tests.fsx` covers Stage 1 movement edge cases, moved actor/fixture map serialization, Stage 2 changed-state assertions, Stage 2H stale visibility-cache behavior, Stage 3 door/look description-key behavior, and LD54 allocation budgets for normal, blocked, door, push, and swap paths.
- `localization-tests.fsx` covers the Stage 3 engine-message localization bridge and missing-key fallback behavior.
- `dotnet build -v:minimal`, `dotnet fsi .\localization-tests.fsx`, `dotnet fsi .\movement-tests.fsx`, `dotnet fsi .\map-tests.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\entity-registry-test.fsx`, `dotnet fsi .\fov-tests.fsx`, `dotnet fsi .\occluder-tests.fsx`, and `dotnet fsi .\persistent-vector-tests.fsx` pass.

## Stage Map

### Stage 0: Shared Engine Contracts

Minimum tier: `[T1]`.

Read the existing engine docs and source, then define the cross-stage result vocabulary: success/failure, blocked causes, message keys, changed cells, changed layer objects, changed local state, visibility-impacting changes, occlusion-impacting changes, and save-relevant changes. This stage is not a gameplay feature; it is the language later stages use to avoid boolean-only APIs.

### Stage 1: Pushable And Moveable Objects

Frontend dependency: Stage 1 pushable objects.

Minimum tier: `[T1]` for source orientation and contract; `[T2]` for edge-case tests once the rule is locked.

Moveables are movement interactions. A move into a moveable object should try the chosen engine rule, mutate map state, and report deterministic result data. Push-or-swap is intentional: push in the movement direction when possible, otherwise swap with the mover if the mover old cell is still open. In simultaneous multi-PC turns, resolve PC movement first, then evaluate push/swap against the resulting occupied cells. The engine must update occupancy, effective opacity, visibility inputs, occlusion inputs, and save-relevant state for the changed object.

### Stage 2: Rendering Foundation Support

Frontend dependency: Stage 2 rendering foundation.

Minimum tier: `[T1]`.

The engine does not render, but it must return enough state-change data for renderers to avoid brittle full redraws. Stage 2 defines and proves the generic changed-state vocabulary with the mutations available by then, especially movement and moveables. Later stages must use the same vocabulary for doors, pickup/drop, scripted wall changes, and lighting-specific data as those features come online. The functional contract and LD54 hardening are complete: player movement uses one internal allocation-lean typed transaction without weakening result facts or exposing a prevalidated bypass.

### Stage 3: Descriptions And Localization Support

Frontend dependency: Stage 3 descriptions and localization.

Minimum tier: `[T1]`.

Engine data should store stable localization keys and return keys plus typed arguments. Runtime logic must branch on typed state, not rendered strings. This stage supplies the key contract and validation/fallback behavior; later stages add the specific item, container, door, rune, wall-tile, and result-message keys for the features they introduce.

### Stage 4: Items, Pickup, Drop, Hidden Inventory, And Containers

Frontend dependency: Stage 4 items, pickup/drop, hidden inventory, and containers.

Minimum tier: `[T1]`.

Before adding Stage 4 persisted state, implement the engine-owned map-versioning and migration foundation in `docs/Map-Migrations.md`. Existing maps are implicit version 0; loading may migrate in memory but never silently rewrites a source file. The first migration preserves legacy `Tile.IsOccupied` cells as an explicit per-cell movement block. This preflight belongs to Stage 4 because frontend Stage 4 cannot depend on an engine Stage 8 foundation.

After the preflight, implement the minimal durable inventory path from `docs/Containers.md`: item definitions, inventory stacks, player inventory, container inventory, pickup-all in the 3x3 neighborhood, drop-head inventory, and conservation-preserving transfers. This stage must support a key inside a container and a rock on the ground.

Item definition identity is gameplay identity, not art identity. Many items can share the same icon, sprite art, weight, value, and stack defaults through authoring templates, but items with different names, descriptions, or engine-owned capability payloads should be distinct item definitions. Keys with different opened lock ids are the first concrete case, not a special identity system. Ordinary placed item copies do not need unique ids unless they carry unique mutable state; mutable placements such as chests, doors, triggers, and moved containers need map-local identity/state.

Moveable containers must derive effective movement weight from base/default container weight plus live inventory contents.

### Stage 5: Locks, Doors, Interaction Routing, And Look Data

Frontend dependency: Stage 5 locks, doors, interaction routing, and look.

Minimum tier: `[T1]`.

Finalize lock ids, key matching, locked-door open/unlock behavior, and storage-agnostic interaction targeting. Doors store lock ids in map-local state; key item definitions advertise which lock ids they open; the player inventory query connects the two. Add look data for all nine tiles in the 3x3 neighborhood centered on the player for content available through Stage 5. Results should use stable ids, facts, and localization keys.

### Stage 6: Rock, Rune, And Reversible Tile Swap

Frontend dependency: Stage 6 scripted map interaction.

Minimum tier: `[T1]`.

Use the rock-on-rune behavior as the first item-placement-triggered reversible tile swap. No special wall-tile engine feature is needed: while the rock is on the rune, swap the linked wall tile to the specified floor/ground tile; when the rock is removed, restore the wall tile. Collision, opacity, interaction facts, visibility inputs, occlusion inputs, and save/load state must change together because they already follow from the active tile state.

### Stage 7: Lighting, Day-Night, And Occlusion Data

Frontend dependency: Stage 7 lighting, day-night, and occluders.

Minimum tier: `[T1]`.

Add only the durable engine-side data and rules needed by the lighting stage: authored light-emitter data if it belongs in content definitions, world-time/day-night state if it belongs in simulation, and coherent visibility/occlusion inputs for doors, moveables, walls, and reversible tile swaps. Presentation curves, lights, shaders, particles, and shadow visuals stay outside the engine.

### Stage 8: Serialization, Scenario Tests, And Hardening

Frontend dependency: Stage 8 slice hardening.

Minimum tier: `[T1]`.

Exercise and harden the migration pipeline established before Stage 4 across every durable authored-map state added by the slice, add missing golden fixtures, add a small scenario fixture, and update docs/XML for new APIs. This stage does not invent map versioning. Save-game state should be a live-state overlay over authored content, compact like a diff where useful, covering moved objects, changed doors, player inventory, container inventories, picked-up/dropped item stacks, trigger latches, and reversible tile swaps.

## Success Criteria

- Each engine stage supplies the engine prerequisites for the matching frontend stage.
- Moveable objects can be moved through engine movement rules, with deterministic blocked reasons.
- Engine APIs return typed result data rather than forcing callers to infer state from booleans.
- Changed cells/entities and visibility/occlusion impacts are explicit enough for frontend rendering invalidation.
- Localization keys and typed args are returned for descriptions and interaction messages.
- A key item can move from container inventory to player inventory without duplication.
- A locked door checks lock id against player inventory and opens only when a matching key is present.
- Visually identical items can still be distinct item definitions when they differ by name, description, or engine-owned capability payload; keys and lock payloads are the first concrete case.
- Moveable-container strength checks account for live contents as well as base/default weight.
- Pickup collects pickable items from the 3x3 neighborhood centered on the player.
- Drop places the head inventory item according to explicit engine rules.
- Look reports all nine nearby tiles using stable ids and localization keys.
- A rock on a rune swaps a linked wall tile to a specified floor/ground tile, and removing the rock restores the wall while updating collision, opacity, visibility inputs, occlusion inputs, and saved state.
- Serialization tests cover old-map defaults and new-state round trips.

## Out Of Scope

- Rendering, sprite drawing, fog textures, shaders, particles, light nodes, shadow visuals, UI widgets, editor panels, input bindings, and cursor behavior.
- Full visible inventory, equipment, item use, stack splitting UI, and item pile presentation.
- A broad scripting language. This slice needs a narrow trigger/effect foundation that can grow later.
- NPCs, combat, stats, dialogue, magic systems, overworld generation, and polished visual effects.

## Source Docs

- `docs/Interactions.md`
- `docs/Containers.md`
- `docs/Map-Migrations.md`
- `docs/Localization.md`
- `docs/Fog-Of-War.md`
- `docs/Occluder-Task.md`
- `docs/Lights.md`
- `PROJECT_INDEX.md`
- `model-tier-notes.md`

## Limitations

This triage assumes the existing interaction, container, localization, FOV, and occluder docs are the design baseline. Workers should inspect source before changing contracts; some items may already be partly implemented under narrower names.

The map migration document defines a required foundation, not authorization to begin source edits. Follow `AGENTS.md`: discuss the post-compaction plan and receive a go-ahead before implementation.

Stage 0 exists only to prevent repeated API/result-shape churn. It should stay small and practical, not become a broad architecture project.

## History

### 2026-07-07

- Created the first engine-local triage note for the Light, Lock, And Look slice.
- Realigned engine stages with frontend stages so coordination follows the same stage numbers across repos.
- Kept shared result/change contracts in Stage 0 and moved generic rendering-invalidation support into Stage 2 instead of leaving it under lighting.
- Paused mid-Stage 1 for compaction. Recorded recovery state in `docs/todo.md`: movement result contract and typed localization args are in, `Player.tryMove` is now the rich movement API, draft moveable push/swap code exists, and `movement-tests.fsx` must be fixed and run before more implementation.
- Advanced Stage 1 to the verification boundary: moveable player movement now uses an atomic push helper, movement results include stable message keys, `movement-tests.fsx` is syntactically repaired with edge cases added, and the project build passes.
- Completed Stage 1: focused movement tests now cover push, swap, bounds, walls, actors, fixtures, diagonal movement, opacity changes, and moved actor/fixture serialization; broad regression tests also pass.
- Recorded design decisions from item/container/save discussion: item art/defaults can be shared without merging gameplay identity, keys are only the first capability-bearing example, ordinary item copies do not need unique ids unless they have mutable instance state, map-local mutable placements need identity/state, moveable containers derive effective weight from live contents, and saves should be live-state overlays over authored content.
- Completed Stage 2: added typed interaction-result APIs for current door interactions, preserved legacy interaction wrappers, tightened movement occlusion hints to reflect effective-opacity changes, and verified changed-state data for movement, moveables, blocked movement, transparent moveables, and door interactions.
- Completed Stage 3: added entity description-key lookup, an `EngineMessage` to `Localizer` bridge with typed args, description-key coverage for `lookAt`, missing-key fallback coverage, and removed description-key-driven door auto-open control flow.
- Clarified moveable ordering for simultaneous multi-PC turns: resolve PC movement first, then evaluate push/swap against resulting occupancy.

### 2026-07-08

- Tier 0 (frontend repo) locked the Stage 2H visibility-ownership verdict; the engine-side item is now `[NOW]` in `docs/todo.md`. The engine's eager `recomputeVisibility` on movement/interactions runs a map-sized-window FOV compute per move (~20-25ms on 300x300) into state no consumer reads — the frontend maintains its own camera-windowed copy. Locked fix: `GameModel.VisibilityState` becomes a derived cache; mutators report `VisibilityInputChanged` only; recompute happens solely through the explicit entry point at the consumer's cadence. Full work order: `../aspectrpg/Design-docs/stage2-work-order.md`, Stage 2H.
- Completed the engine-side Stage 2H work: eager visibility recomputes were removed from model creation, movement, auto-interaction, and visibility setters; `docs/Fog-Of-War.md` now records the derived-cache lifecycle law; `movement-tests.fsx` pins empty-at-create and stale-until-explicit-recompute behavior; the Debug build post-build copied artifacts into the frontend repo.

### 2026-07-10

- Added the LD54 movement-hardening follow-up without changing the Stage 4 current focus. The source audit exonerates engine movement from frontend rung-0 hitches and finds no map-size-scaling loop, while recording allocation and duplicate-validation debt to remove before NPC/AI movement increases call volume.
- Completed LD54: one internal map transaction now classifies and commits player normal/push/swap movement, scalar snapshots report opacity changes, blocked payloads and the boolean wrapper avoid rich allocations, and byref registry/property probes remove hot tuple/option wrappers. The rich normal path fell from approximately 1,744 to 552 bytes per accepted call and from 136 to 0 for wall blocks; all focused and broad engine suites pass.
- Made map format versioning and migration the Stage 4 preflight. Added `docs/Map-Migrations.md`, classified existing maps as implicit version 0, and defined legacy `Tile.IsOccupied` preservation as the first migration rather than deleting compatibility data.
- Clarified the terminal state: compatibility is preserved through the version-0 migration, `IsOccupied` is then removed from current models and authoring, and all authored project maps are explicitly upgraded rather than left indefinitely on the legacy format.
