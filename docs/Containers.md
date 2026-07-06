# Containers, Inventories, And Item Representation

This document explains the intended engine-side foundation for containers, player inventory, item definitions, keys, locks, and the first locked-door gameplay slice.

The short version: containers and players should share one inventory storage model, but they should not share behavior through inheritance. Item definitions should be small, concrete, and safe to expand. The first useful item kinds are `Key`, `Weapon`, and `Junk`. Containers are map-local interactive state. Player inventory is game-local state. Doors should recognize keys through lock ids, not through sprite ids, hardcoded coordinates, or frontend state.

## Current Behavior

There is no live inventory system yet.

The engine currently has item-shaped data in the layer system. `LayerCell.Items` stores item entity ids on a map tile, and `ItemProperties` stores only a description key. This is enough for map placement, rendering hints, look-at text, and item-like layer content, but it is not an inventory model.

The interaction system also has only partial container-shaped data:

- `TileType.Container` exists.
- fixture definitions can be marked `Interactable`.
- `docs/Interactions.md` defines the immediate gameplay target: a container holding a key that opens a locked door.
- `GameModel.TileComplexStateInstance` can store closed-door lock state by tile index, but there is no broad map-local state store for containers, item contents, or acquired inventory.

This means the next step should not bolt container behavior onto the existing layer item list. Layer items are world placement. Inventory entries are ownership state. Those concepts can exchange items, but they should not be the same storage.

## Item Definitions

An item definition is immutable content data saying what an item is. It is shared by every occurrence of that item.

The first base should stay concrete:

```fsharp
type ItemKind =
    | Key of opensLocks: int[]
    | Weapon of weaponProfileId: int
    | Junk

type ItemDefinition =
    { Id: int
      NameKey: string
      DescKey: string
      Visual: SpriteRef
      Weight: int
      Value: int
      StackLimit: int
      Kind: ItemKind }
```

`Id` is the stable item definition id.

`NameKey` and `DescKey` are localization keys. The engine should pass keys outward; the frontend chooses how to present names and descriptions.

`Visual` is the renderer-facing sprite reference for the item. The engine does not load textures or draw it. If a later inventory UI needs a distinct icon, that can be added intentionally as a second visual reference rather than guessed from this field.

`Weight` is an integer game unit. It should not be a float in the base model. The game can decide later whether weight affects carrying capacity, movement, stamina, encumbrance, or only UI text.

`Value` is the base economic value. It is content data, not a shop final price. Local economy, faction, condition, and barter modifiers can live elsewhere later.

`StackLimit` controls inventory stacking. Rocks can stack. Keys and swords usually do not. A `StackLimit` of `1` means each inventory slot can hold only one item of that definition unless a later policy explicitly says otherwise.

`Kind` carries the small kind-specific payload. This is the F#-native replacement for inheritance. A key knows what lock ids it opens. A weapon points to a separate weapon profile. Junk has no special engine behavior.

## Concrete Examples

A rock is the baseline item. It can be picked up, stacked, weighed, valued, and shown:

```fsharp
let smallRock =
    { Id = 1000
      NameKey = "item.rock.small.name"
      DescKey = "item.rock.small.desc"
      Visual = SpriteRef.SheetCell(SpriteSheetCell(2, 0, 0))
      Weight = 2
      Value = 0
      StackLimit = 99
      Kind = ItemKind.Junk }
```

A key is still an ordinary item definition, but its kind carries lock ids:

```fsharp
let brassKey =
    { Id = 1001
      NameKey = "item.key.brass.name"
      DescKey = "item.key.brass.desc"
      Visual = SpriteRef.SheetCell(SpriteSheetCell(2, 0, 4))
      Weight = 1
      Value = 0
      StackLimit = 1
      Kind = ItemKind.Key [| 2001 |] }
```

A sword is an ordinary item definition whose combat behavior is deferred to a weapon profile:

```fsharp
let ironSword =
    { Id = 1101
      NameKey = "item.weapon.iron_sword.name"
      DescKey = "item.weapon.iron_sword.desc"
      Visual = SpriteRef.SheetCell(SpriteSheetCell(2, 1, 0))
      Weight = 6
      Value = 25
      StackLimit = 1
      Kind = ItemKind.Weapon 3001 }
```

The sword definition should not directly carry every combat field. Weapon balance will grow differently from inventory rules. The item definition points at a weapon profile; the weapon/combat system owns damage, range, accuracy, speed, handedness, durability policy, and similar combat-specific data.

Torches are intentionally not a fourth category yet. A torch can begin life as `Junk` if it is only a pickup object. When lit torches become real gameplay objects, they can gain item instance state and presentation links to `docs/Lights.md` and `docs/Effects.md`, or `ItemKind` can gain a new case if torch behavior becomes engine-owned. Do not add a torch category merely because torches are visually interesting.

## Item Stacks And Instances

Inventory storage should be compact and mutable in place.

```fsharp
[<Struct>]
type ItemStack =
    { ItemId: int
      Quantity: int
      InstanceId: int }
```

`ItemId` points to `ItemDefinition.Id`.

`Quantity` records how many copies are in this stack.

`InstanceId` is `0` when no per-instance state is needed. Most rocks do not need unique identity. Most ordinary keys do not need unique identity. A lit torch with remaining fuel, a damaged sword, an enchanted weapon, or a quest item with custom state can use a non-zero instance id later.

The important rule is that unique item state is opt-in. Do not allocate per-instance state for every item just because some future item might need it.

Two stacking rules must be locked now, because getting them wrong fails silently later:

- Stacks may merge only when they share the same `ItemId` **and** both have `InstanceId = 0`, and the merged quantity respects `StackLimit`. Merge logic keyed on `ItemId` alone will quietly collapse two unique items into one and lose the second item's instance state, and no add/remove test will notice.
- A non-zero `InstanceId` implies `Quantity = 1`. Instance state describes one physical item; a stack of five cannot share one damage value.

## Inventory State

Inventory is storage, not behavior inheritance.

A practical first shape is:

```fsharp
type InventoryState =
    { mutable Slots: ItemStack[] }
```

The slots array is one stable heap object owned by the player, a container, or another inventory owner. Individual stacks are structs so ordinary add/remove operations mutate compact values rather than creating many small heap objects.

In F#, "mutate in place" here means replacing the whole struct element: `slots.[i] <- { slots.[i] with Quantity = q }`. The `ItemStack` fields themselves stay immutable. This is still allocation-free because the struct is copied by value into the array slot. Do not add `mutable` fields to the struct or promote it to a class to chase field-level mutation; whole-element replacement is the intended idiom.

This is acceptable for a game engine because inventories are long-lived state, not per-frame visibility or per-cell renderer data. The performance rule is simple: keep definitions shared, keep stacks value-shaped, avoid allocating during hot map scans, and mutate inventory slots in place.

Player inventory and container inventory can then compose the same storage:

```fsharp
type PlayerInventoryState =
    { Inventory: InventoryState }

type ContainerState =
    { IsOpen: bool
      Inventory: InventoryState }
```

That is cleaner than inheritance. The shared part is the inventory. The different behavior belongs beside it. A player may later gain equipment slots, carry limits, gold, quick-use slots, or UI sorting. A container may gain locked state, trap state, one-shot refill rules, ownership flags, or restock rules. Those do not need a shared base class.

## Containers

A container is an interactable map object with map-local state.

The same chest definition can appear ten times on one map. Opening one chest must not open the other nine. Therefore container state must be keyed by a map-local object id or by a carefully defined interaction target, not by shared fixture id or sprite id.

The minimum useful container state is:

```fsharp
type ContainerState =
    { IsOpen: bool
      Inventory: InventoryState }
```

`IsOpen` is runtime/save state. It does not need to mean visible lid animation. The frontend may choose an animation or sprite swap after the engine reports the state change.

The container inventory is also runtime/save state. Authored content can initialize it, but live play owns removals and additions.

Repeated interaction with an empty container must not duplicate items. The inventory state is the source of truth.

## Player Inventory

Player inventory is game-local state owned by the live game model or future save stream.

The first slice does not need a visible inventory UI. The engine can still model the key as a real inventory item. That is better than a separate key-ring shortcut because the same representation will support picking up rocks, swords, torches, quest items, and container loot later.

For the first implementation, a helper query can answer:

```text
does player inventory contain an item whose ItemKind.Key opens this lock id?
```

The frontend does not need to inspect item stacks to decide whether a door opens. It should call the engine interaction entry point and present the result.

## Keys, Locks, And Doors

Doors should not recognize keys by sprite id, item placement id, frontend node path, or hardcoded map coordinate.

A locked door owns or references a lock id:

```fsharp
type DoorLockState =
    { IsLocked: bool
      LockId: int option }
```

A key item advertises which lock ids it opens:

```fsharp
ItemKind.Key [| lockId |]
```

Door interaction checks the player's inventory for any key item whose `opensLocks` array contains the door's `LockId`.

This supports common cases without special casing:

- one key opens one door
- one key opens several doors
- several keys open the same lock
- a master key opens a group of locks
- a door can be locked without a known key in the current map

The first behavior can be:

```text
interact with locked door
if player inventory has matching key:
    unlock and open the door
else:
    return locked message key
```

The exact pacing can be tuned later. Some games unlock and open in one interaction; others unlock first and require a second interaction to open. The important foundation is that lock recognition goes through item definitions and player inventory, not through frontend-only state.

## First Gameplay Slice

The first robust slice is:

```text
container contains brass key
door is locked with brass lock id
player interacts with container
engine transfers brass key stack into player inventory
container inventory loses the key
player interacts with locked door
engine finds key by lock id
door unlocks and opens
```

This is small, but it hardens the permanent base:

- item definitions exist
- inventory stacks exist
- player inventory exists
- container inventory exists
- container contents are map-local
- lock ids connect keys to doors
- repeated container interaction cannot duplicate loot
- the frontend presents result keys but does not own the rule

## Authoring And Serialization

The runtime data format should remain FlatBuffers, following the broader interaction design.

There are three kinds of data to serialize eventually:

- item definitions
- authored initial inventories for containers or other owners
- live inventory state in a save stream

Item definitions are content. They can live in an item registry stream or in a broader entity/content registry. The current `EntityRegistry.SpriteProps` item case is not enough because it only stores description and sprite/entity data. The item definition needs inventory and gameplay fields such as weight, value, stack limit, and kind.

Authored container contents are map content. They can be produced by editor UI or by F# builder scripts, then compiled into the same binary bundle style described in `docs/Interactions.md`.

Live inventory state is save data. A runtime map serializer should not silently become the full save stream. Opened containers, removed items, player inventory, item instance state, and door locks should be saved as live game state when that stream exists.

## Godot Frontend Consumption

The frontend should treat inventory and container interaction as engine-owned gameplay state.

For a container:

```text
player presses interact
Godot calls engine interaction entry point
engine resolves the container target
engine transfers item stacks according to rules
engine returns message keys and changed-state hints
Godot presents sounds, UI, animation, particles, or popups
```

For a locked door:

```text
player presses interact near door
engine checks door lock state
engine checks player inventory for a matching key
engine mutates door state if allowed
Godot updates visuals from returned state/map changes
```

Godot should not duplicate the rule "this key opens this door." It can display the key, play a sound, animate the door, or show a message. The decision belongs in the engine.

## Design Boundaries And Operating Rules

Use composition, not inheritance. A player and a container can both own `InventoryState`; they do not need to inherit from a common inventory owner class.

Keep item definitions immutable and shared. Do not put live quantity, ownership, durability, fuel, or opened/emptied state in the shared item definition.

Keep item stacks compact. `ItemStack` should be value-shaped, with per-instance state only when an item truly needs it.

Keep categories small. The first item kind set is `Key`, `Weapon`, and `Junk`. Add a new kind only when the engine owns meaning for it.

Do not turn `Junk` into a judgment about game importance. Rocks, torches, ingredients, quest clutter, and readable props may all start as junk if they have no special engine behavior yet.

Do not store container contents on a reusable fixture definition. Contents are authored per placement and mutate per map/save.

Do not key doors to item visuals. Doors recognize lock ids; keys advertise lock ids.

Transfers conserve items. Moving a stack between inventories is one remove plus one add, and the pair must be all-or-nothing. Removing without adding silently destroys items; adding without removing duplicates them; both bugs pass ordinary per-inventory tests. The transfer test oracle is total quantity per item id summed across all inventories and world placements, compared before and after every operation — including failed and partial-capacity transfers. The oracle may not inspect slot order or slot indices; those are free to change.

The engine is not a renderer. Item visuals, container animations, torch particles, and sword draw states are frontend presentation contracts. Item ownership, stack counts, and lock matching are engine/game data.

## Future Work

Add an item definition registry with explicit ids, validation, and FlatBuffers serialization.

Decide whether item definitions live beside entity registry data or in a separate item registry stream. The current entity registry item shape is too small for inventory semantics.

Add `InventoryState` operations: add stack, remove quantity, transfer between inventories, find matching key, count item, and compact slots.

Add focused tests for stack limits, transfer behavior, failed transfers, empty containers, repeated interactions, and key lookup by lock id.

Add map-local container identity through the interaction target/state system.

Add the first container/key/locked-door implementation slice described above.

Add player inventory to `GameModel` or the next broader game-state container without making `TileMap` own player inventory.

Add live save/load for player inventory, container inventories, door lock state, and future item instance state.

Add item instance state for mutable unique items such as lit torches, damaged weapons, charged magical items, or named quest items.

Connect torch-like item presentation to `docs/Lights.md` and `docs/Effects.md` only when the engine has a clear rule for carried, dropped, lit, extinguished, or equipped torches.

Add weapon profile data once combat rules need it. Keep combat stats out of the base item definition until the weapon system exists.

Add frontend-facing changed-state hints for inventory/container interactions, such as acquired item ids, removed item ids, opened container target, failed lock key, and message keys.

## Source Map

- `Types.fs` - current `TileType.Container`, `TileType.Door`, `ComplexState`, and door lock state shape.
- `LayerGrid.fs` - current `ItemProperties`, `SpriteType.Item`, `LayerCell.Items`, and entity registry data.
- `Maps.fs` - current world item placement through `TileMap.AddItem` and render item views.
- `MapEditor.fs` - current editor item placement and layer conversion.
- `GameState.fs` - current `GameModel`, door lock helpers, interaction dispatch, and nearby interact-key scan.
- `docs/Interactions.md` - interaction target model, map-local state, container/key slice, triggers, and authoring ladder.
- `docs/Lights.md` - future torch or item light presentation contract.
- `docs/Effects.md` - future torch flame, smoke, spell, and item particle presentation contract.
- future item registry serializer - binary item definitions.
- future interaction/inventory state serializer - authored container contents and live save state.

## Historical Notes

### 2026-07-06

- Added the first containers and inventory design document.
- Chose composition over inheritance: player inventory and container inventory share `InventoryState`, while player/container-specific behavior lives beside that state.
- Chose a small concrete item model: shared `ItemDefinition`, F# union `ItemKind`, compact `ItemStack`, and optional future item instance state.
- Limited the first item kinds to `Key`, `Weapon`, and `Junk`.
- Recorded concrete examples for rocks, brass keys, and iron swords.
- Recorded that doors should recognize keys through lock ids advertised by `ItemKind.Key`, not through sprite ids or frontend state.
- Recorded that torches should not force a new category yet; torch light and particle behavior can connect to the existing light/effect design once carried or lit torch rules exist.

### 2026-07-06 — tier-0 design review

- Verdict: the component approach is approved as written. Composition over inheritance, shared `InventoryState`, the concrete `ItemKind` union, opt-in instance state, and lock-id key recognition are locked decisions; implement without relitigating.
- Locked two stacking rules in "Item Stacks And Instances": stacks merge only on matching `ItemId` with both `InstanceId = 0`, and non-zero `InstanceId` forces `Quantity = 1`. Merge logic keyed on `ItemId` alone loses instance state silently.
- Clarified in "Inventory State" that struct mutation means whole-element replacement (`slots.[i] <- { ... }`); `ItemStack` fields stay immutable, and the struct must not grow mutable fields or become a class.
- Added the item-conservation rule and its mandated test oracle to "Design Boundaries And Operating Rules": total quantity per item id across all inventories is invariant under transfers, including failed ones; the oracle may not compare slot order.
