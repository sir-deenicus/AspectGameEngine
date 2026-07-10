# Todo

This is the active engine todo list. The current priority lens lives in `docs/triage.md`.

## Fresh-Context Recovery Note

As of 2026-07-08, the engine-side Stage 2H visibility ownership work is complete in this repo: `GameModel.VisibilityState` is a derived cache, mutators report `Changes.VisibilityInputChanged` without eager recompute, and the Debug build post-build copied the DLL/PDB/XML into `../aspectrpg/Scripts/GameEngine/`. The frontend Stage 2H source swap and Godot-side oracles remain in the frontend repo.

Stage 4 has not been approved for implementation yet. The next engine feature step is discussion of the Stage 4 plan before any source edits. The latest user correction is that the raw-English message/string audit belongs in Stage 4, not Stage 8 and not reopened Stage 3. Stage 0 through Stage 3 remain complete. An accidental Stage 4 implementation scaffold was started and removed; do not assume any Stage 4 source work exists. `archive/decal-movement-repro.fsx` may exist as an archived, untracked repro script from the layer-0 decal investigation.

Use this item format:

```text
- [ ] [NOW] [T1] Work item.
```

Status tags are `[NOW]`, `[TODO]`, `[BLOCKED]`, `[DONE]`, and `[DEFERRED]`. Tier tags are minimum executor tiers from `model-tier-notes.md`, not priority labels.

## Current Slice: Light, Lock, And Look Engine Base

Goal: provide the engine prerequisites for each matching frontend stage without mixing in frontend work. Frontend Stage N may depend on engine Stage 0 through engine Stage N, but not on later engine stages. The engine supplies durable state, movement/interaction rules, typed result data, localization keys, visibility/occlusion inputs, serialization, and tests.

## Stage 0: Shared Contracts

- [x] [DONE] [T1] Read `AGENTS.md`, `PROJECT_INDEX.md`, `PROJECT_ETHOS.md`, `docs/Interactions.md`, `docs/Containers.md`, `docs/Localization.md`, `docs/Fog-Of-War.md`, and `docs/Occluder-Task.md` before implementation.
- [x] [DONE] [T1] Inspect `GameState.fs`, `Player.fs`, `Objects.fs`, `Maps.fs`, `LayerGrid.fs`, `Types.fs`, `FOV.fs`, `ChunkOcclusionManager.fs`, and serializers for existing support.
- [x] [DONE] [T1] Define a common movement/interaction result shape carrying success, blocked cause, message key, changed cells, changed entities, visibility-impact flags, occlusion-impact flags, and save-relevant state changes. Movement uses `MovementResult`; current door interactions use `InteractionResult`.
- [x] [DONE] [T1] Decide which current boolean-return APIs stay as wrappers and which new typed APIs become the slice contract. `Player.tryMove` is typed and `Player.tryMoveBool` is the wrapper; `GameUpdate.interactAtResult`, `interactAutoAtResult`, and `tryForInteractionsResult` are typed interaction APIs with legacy wrappers preserved.

## Stage 1: Pushable And Moveable Objects

Frontend dependency: Stage 1 pushable objects.

- [x] [DONE] [T1] Confirm the existing `Objects.fs` helper behavior against the desired moveable rule in `docs/Interactions.md`, including intentional push-or-swap behavior and simultaneous multi-PC ordering where PC movement resolves before moveable push/swap.
- [x] [DONE] [T1] Implement or expose movement-driven moveables through player movement, not a separate interaction-only path. `Player.tryMove` handles movement into moveable fixtures.
- [x] [DONE] [T1] Make moveable results report moved object identity, old position, new position, player old/new position, blocked cause, and message key.
- [x] [DONE] [T1] Ensure moveable updates refresh occupancy, effective opacity, FOV inputs, occluder inputs, and save-relevant map state.
- [x] [DONE] [T2] Add tests for push forward, swap fallback if retained, blocked by bounds, blocked by wall, blocked by actor, blocked by fixture, diagonal movement, opacity-changing moveable, and save/load after movement.

## Stage 2: Rendering Foundation Support

Frontend dependency: Stage 2 rendering foundation.

- [x] [DONE] [T1] Make movement and interaction results identify changed base cells and changed layer cells/entities.
- [x] [DONE] [T1] Make movement and interaction results identify whether visibility/FOV inputs changed.
- [x] [DONE] [T1] Make movement and interaction results identify whether occlusion inputs changed.
- [x] [DONE] [T1] Prove the changed-state vocabulary with movement and moveable mutations available by Stage 2; later stages must reuse it for their own mutations.
- [x] [DONE] [T2] Add Stage 2 tests that mutation results report expected changed cells/entities for movement and push/moveable behavior.
- [x] [DONE] [T1] Execute the Stage 2H engine change from the frontend repo's locked work order (`../aspectrpg/Design-docs/stage2-work-order.md`, Stage 2H, LD21-LD25): make `GameModel.VisibilityState` a derived cache — remove eager `recomputeVisibility` calls from `finishPlayerMove`, `tryForInteractionsResult`, `setVisibilityWindow`, `setVisibilityTranslucencyBudget`, and `createWith`; mutators only report `Changes.VisibilityInputChanged`. Update existing tests to explicit recompute (re-adding eager computes to green a test is forbidden), add the LD24 staleness test (post-move: `VisibilityInputChanged = true` and `VisibilityState` bit-identical), document the lifecycle law in `docs/Fog-Of-War.md`, rebuild and vendor the DLL into the frontend repo. Context: the map-sized default window makes the current eager compute cost ~20-25ms per move step on a 300x300 map, written into state nothing reads.

## Stage 3: Descriptions And Localization Support

Frontend dependency: Stage 3 descriptions and localization.

- [x] [DONE] [T1] Define the description-key contract for base tiles, fixtures, decals, actors, and generic entity/result data; later stages apply it to items, containers, doors, runes, and wall tiles as those features arrive.
- [x] [DONE] [T1] Use stable keys and typed args for Stage 1-3 results, especially movement/moveable and look/description scaffolding; later stages add their feature-specific keys under this contract.
- [x] [DONE] [T1] Keep runtime logic branching on typed state rather than rendered strings.
- [x] [DONE] [T2] Add coverage for missing keys, fallback behavior, message args, and base tile/entity description keys.

## Stage 4: Items, Pickup, Drop, Hidden Inventory, And Containers

Frontend dependency: Stage 4 items, pickup/drop, hidden inventory, and containers.

- [ ] [NOW] [T1] Audit Stage 4-facing engine messages and strings so runtime results never expose raw English prose; replace cases like blocked/block text with stable localization keys and typed args.
- [ ] [NOW] [T1] Add or finalize item definitions with stable ids, name key, description key, visual reference, weight, value, stack limit, and `ItemKind`; allow authoring templates/defaults so many gameplay-distinct items can share art, weight, value, and stack policy while remaining distinct definitions when names, descriptions, or engine-owned capability payloads differ.
- [ ] [NOW] [T1] Add `InventoryState` and `ItemStack` operations: add, remove, transfer, count, compact, and find matching key.
- [ ] [NOW] [T1] Enforce locked stacking rules: merge only when `ItemId` matches and both `InstanceId = 0`; non-zero `InstanceId` implies quantity `1`.
- [ ] [NOW] [T1] Keep placed item identity opt-in: ordinary copies use item definitions and stack quantities; reserve non-zero `InstanceId` for unique mutable items and map-local object ids for mutable placements like chests, doors, triggers, and moved containers.
- [ ] [NOW] [T1] Add player inventory to live game state without putting inventory inside `TileMap`.
- [ ] [NOW] [T1] Add container inventory as map-local state, not reusable fixture definition state.
- [ ] [NOW] [T1] Make moveable-container effective weight derive from base/default container weight plus live inventory contents.
- [ ] [NOW] [T1] Implement pickup-all from the 3x3 neighborhood centered on the player, including the player's tile.
- [ ] [NOW] [T1] Implement drop-head inventory with explicit placement and blocked-drop rules.
- [ ] [NOW] [T1] Preserve item conservation across pickup, drop, and transfers; tests should compare total quantity per item id across world, containers, and player inventory.
- [ ] [NOW] [T2] Add tests for key in container, repeated container interaction, empty container, rock on ground, pickup adjacent, pickup current tile, drop head, blocked drop, stack limit, and failed transfer.

## Stage 5: Locks, Doors, Interaction Routing, And Look Data

Frontend dependency: Stage 5 locks, doors, interaction routing, and look.

- [ ] [TODO] [T1] Finalize the lock id contract between key item definitions and map-local door lock state; doors store lock ids, keys advertise opened lock ids, and shared item art/defaults do not imply shared gameplay identity.
- [ ] [TODO] [T1] Make locked-door open/unlock logic check player inventory for a key whose `ItemKind.Key` opens the lock id.
- [ ] [TODO] [T1] Ensure door open/close paths respect locked state consistently, including auto-open during movement.
- [ ] [TODO] [T1] Define a storage-agnostic `InteractionTarget` shape covering base tile, fixture, actor, item, and decal targets.
- [ ] [TODO] [T1] Make interaction target ordering deterministic and documented in tests.
- [ ] [TODO] [T1] Add or expose a look query for all nine tiles in the 3x3 neighborhood centered on the player.
- [ ] [TODO] [T1] Return typed look facts and stable localization keys for terrain, fixtures, decals, items, actors, containers, doors, visibility, and opacity state.
- [ ] [TODO] [T2] Add tests for locked door without key, wrong key, matching key, repeated interaction, blocked open, save/load after unlock, look ordering, empty tile, item tile, container tile, door tile, occupied tile, opaque tile, explored tile, and unseen tile.

## Stage 6: Rock, Rune, And Reversible Tile Swap

Frontend dependency: Stage 6 scripted map interaction.

- [ ] [TODO] [T1] Confirm the existing tile/state mutation path can express the item-placement condition without adding special wall-tile support.
- [ ] [TODO] [T1] Represent the rune link durably: target wall tile coordinate/id, original wall tile, and replacement floor/ground tile chosen by us.
- [ ] [TODO] [T1] Make rock present on the rune swap the linked wall tile to the specified floor/ground tile and return changed-state and message-key data.
- [ ] [TODO] [T1] Make rock removal restore the linked wall tile and return changed-state and message-key data.
- [ ] [TODO] [T2] Add tests for rock on rune, wrong item on rune, repeated trigger, rock removed, wall-to-floor tile identity, restored wall identity, before/after collision, before/after opacity, and save/load during both states.

## Stage 7: Lighting, Day-Night, And Occlusion Data

Frontend dependency: Stage 7 lighting, day-night, and occluders.

- [ ] [TODO] [T1] Decide whether authored light-emitter data belongs in tile/entity definitions or a separate durable content stream.
- [ ] [TODO] [T1] Decide whether world time/day-night state is simulation state in `GameModel` or external presentation state; implement only if it belongs in the engine.
- [ ] [TODO] [T1] Ensure door, moveable, and reversible tile-swap changes expose occlusion and light-emitter change hints through the Stage 2 result vocabulary.
- [ ] [TODO] [T2] Add tests for FOV/occlusion after door open, moveable moved, wall-to-floor tile swap/restoration, and save/load of any added light/time data.

## Stage 8: Serialization, Scenario Tests, And Hardening

Frontend dependency: Stage 8 slice hardening.

- [ ] [TODO] [T1] Version new FlatBuffers schema changes deliberately and document defaults for older maps.
- [ ] [TODO] [T1] Define the save-game live-state overlay/diff shape over authored content, covering moved objects, door open/lock state, player inventory, container inventories, picked-up/dropped item stacks, trigger state, and tile-swap state.
- [ ] [TODO] [T1] Add round-trip tests for moveables, item definitions or references, player inventory, container inventory, locks, door state, rune tile-swap state, and any light/time data added.
- [ ] [TODO] [T1] Add a small engine scenario fixture covering container with key, locked door, moveable object, rock, rune, wall-to-floor tile swap/restoration, opacity-changing state, and localization keys.
- [ ] [TODO] [T2] Update XML docs or focused docs for any new public APIs. Priority case: `EngineChangeSet`, `MovementResult.Changes`, and `InteractionResult.Changes` have no XML doc comments — a frontend Tier 0 audit (aspectrpg `Design-docs/stage2-work-order.md`, 2026-07-07) surveyed the shipped XML surface, concluded the change-set API did not exist, and initially designed a redundant Godot-side diff around its absence. Consumers discover this API only by reading F# source; doc-comment the change-set contract so the XML surface reflects it.

## Deferred

- [ ] [DEFERRED] [T1] Full save-game stream beyond the minimal live state needed for this slice.
- [ ] [DEFERRED] [T1] General scripting language or trusted runtime script hooks.
- [ ] [DEFERRED] [T1] NPC, combat, stats, dialogue, magic, and overworld systems.
- [ ] [DEFERRED] [T2] Equipment, item use, stack splitting commands, and visible inventory behavior.
- [ ] [DEFERRED] [T2] Presentation concerns such as rendering, UI, input binding, shaders, particles, light visuals, and shadow visuals.

## Completed

- [x] [DONE] Documented current interaction direction in `docs/Interactions.md`.
- [x] [DONE] Documented current container, inventory, item, key, and lock direction in `docs/Containers.md`.
- [x] [DONE] Documented current localization contract in `docs/Localization.md`.
- [x] [DONE] Documented current FOV and visibility contract in `docs/Fog-Of-War.md`.

## Limitations

This todo is scoped to engine work only. It intentionally excludes frontend presentation, editor UI, renderer cleanup, input binding, and visual effects.

Some tasks may already be partially implemented. Workers should verify the current source before adding new types or APIs.

## History

### 2026-07-08

- Completed the engine-side Stage 2H visibility ownership work: `GameModel.VisibilityState` is now a derived cache, creation starts with an empty cache, visibility setters are field-only, player movement and auto-interaction no longer recompute eagerly, and `movement-tests.fsx` pins stale-cache behavior until explicit `GameUpdate.recomputeVisibility`.
- Verified the Debug build and post-build frontend vendoring, then ran `dotnet fsi .\movement-tests.fsx`, `dotnet fsi .\tests.fsx`, `dotnet fsi .\map-tests.fsx`, `dotnet fsi .\localization-tests.fsx`, `dotnet fsi .\entity-registry-test.fsx`, and `dotnet fsi .\fov-tests.fsx`.

### 2026-07-07

- Created the first engine-local todo list for the Light, Lock, And Look slice.
- Realigned stage numbers with the frontend plan, with Stage 0 reserved for shared contracts and Stage 2 carrying generic rendering-foundation change hints.
- Recorded item identity and save-state decisions from design discussion: visually identical items with different engine-owned capability payloads are distinct item definitions, keys are the first concrete case, ordinary placed item copies do not need unique ids, mutable placements need map-local identity, moveable containers derive effective weight from contents, and saves should be live-state overlays over authored content.
- Advanced Stage 1 movement work to the test boundary: moveable player movement now uses an atomic push helper, movement results include stable message keys, and `movement-tests.fsx` has the optional-argument syntax fixed plus edge cases.
- Completed Stage 1 verification: focused movement tests and broad regression tests pass, including moved actor/fixture serialization coverage.
- Completed Stage 2 rendering-foundation support: movement and current door interactions return typed change sets with changed base cells, changed layer cells, changed entities, visibility/FOV hints, occlusion hints, and save relevance. Legacy interaction wrappers remain available.
- Completed Stage 3 descriptions and localization support: description keys are exposed for base tiles and registered entities, `EngineMessageLocalization` renders typed engine messages through `Localizer`, door auto-open no longer branches on description keys, and focused coverage verifies fallback keys, typed args, look description keys, and description-independent door movement.
- Clarified moveable ordering for simultaneous multi-PC turns: push-or-swap is intentional, and push/swap checks run after PC movement has claimed resulting occupied cells.
- Corrected the raw-English message/string audit into Stage 4. Stage 3 remains complete, Stage 8 no longer owns that audit item, and Stage 4 implementation still needs discussion before source edits.
