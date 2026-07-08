# Localization

This document explains how the current localization system works and what contract the engine, game layer, editor, and Godot frontend should share. The implementation lives in `Localization.fs`.

The short version: localization is key-driven content data. Engine systems should store and return stable localization keys plus typed arguments, not already-rendered English text. `.agl` source is an authoring format, `AGLB1` binary packs are the runtime format, and `Localizer` is the small lookup/rendering helper for the game or frontend to use when it is time to present text.

## Explanation

The localization pipeline has three layers.

Authoring starts with `.agl` text. A file can contain metadata, simple messages, formatted placeholders, plural or select variants, aliases, comments, and developer notes. The parser turns that source into an `AglFile`.

Packing turns an `AglFile` into an `AglPack`. The pack keeps keys in source order, builds an ordinal key-to-index dictionary, and stores message data in arrays. It can then be written to an `AGLB1` binary blob for runtime loading.

Runtime presentation uses `Localizer`. The localizer accepts one primary pack and optional fallback packs. Lookups search the primary pack first, then fallback packs in order. Missing keys return the key itself from `Get`, or `ValueNone` from `TryGet`, which makes missing localization visible during development instead of silently hiding it.

Localization should not be used as a hidden global service inside engine constructors. Content and gameplay systems should carry keys. The system that owns presentation should choose the active locale, load the packs, and format the final string.

## Authoring Syntax

Simple entries use a stable key and a quoted message:

```text
ui.hello = "Hello, World!"
item.rock.name = "Rock"
item.rock.desc = "A small stone."
```

Placeholders use `{name}`. Optional .NET format strings use `{name:format}`:

```text
ui.greeting = "Hello, {name}!"
shop.price = "Price: {amount:C2}"
```

Plural entries are currently recognized by using `count` as the selector name:

```text
inv.items(count) {
  =0: "No items"
  =1: "One item"
  other: "{count} items"
}
```

Select entries use any other selector name:

```text
char.pronoun(gender) {
  male: "he"
  female: "she"
  other: "they"
}
```

Every plural or select block must include `other`. Exact numeric labels such as `=0` are supported for plural-like entries. Category labels are case-insensitive at runtime when selecting a variant.

Aliases point one key at another key:

```text
ui.quit = "Quit"
menu.exit = @ui.quit
```

Triple-quoted strings are available for multiline text:

```text
lore.intro = """
Line 1
Line 2
"""
```

Metadata is optional:

```text
[meta]
locale = "en-US"
fallback = ["en"]
```

If no locale is provided, packed data uses `und` as the locale code.

## Data It Owns

`AglFile` is the parsed authoring model:

```fsharp
type AglFile =
    { Meta: Meta
      Entries: Entry array }
```

Entries keep the key, the parsed message, and an optional developer note. Notes are authoring information; they are not written into the runtime pack today.

`Message` is the core localized payload:

```fsharp
type Message =
    | Simple of pieces: SimpleMessage
    | Plural of selector: string * variants: Variant array
    | Select of selector: string * variants: Variant array
    | Alias of target: Key
```

`SimpleMessage` is a sequence of text and holes. A hole is a named placeholder with an optional format string.

`AglPack` is the runtime lookup model:

```fsharp
type AglPack =
    { Locale: string
      Keys: string array
      Index: IReadOnlyDictionary<string, int>
      Messages: Message array }
```

The pack does not own the active locale choice, language fallback policy, or UI display rules. It is just the compiled content bundle for one locale.

## Runtime Flow

The intended build/editor flow is:

```text
.agl source
parse with AglParser.Parse
build with AglPacker.Build
write with AglPacker.WriteBinary
ship or cache AGLB1 bytes
```

The intended game/frontend flow is:

```text
load primary locale bytes
load optional fallback locale bytes
create Localizer
receive keys and args from engine/game systems
render strings at the presentation boundary
```

The public convenience helpers cover the common paths:

```fsharp
Localization.loadAgl text
Localization.loadAglFile path
Localization.fromBinary bytes
Localization.fromBinaryWithFallbacks primary fallbackBytes
```

Runtime lookup methods are:

```fsharp
localizer.TryGet key
localizer.Get key
localizer.Format(key, args)
localizer.Plural(key, count, args)
localizer.Select(key, label, args)
```

Aliases are resolved iteratively and are cycle-safe. Alias cycles return the original key rather than looping forever.

## Key Ownership

Engine data should own keys where the text belongs to reusable content:

- `TileProperties.DescriptionKey` owns tile description text.
- `ActorProperties.DescKey`, `FixtureProperties.DescKey`, `ItemProperties.DescKey`, and future item definitions own entity text keys.
- Container and inventory definitions should own `NameKey` and `DescKey`.
- Interaction results should return message keys such as "door locked", "container empty", or "took item".

Map-local state should not store localized strings. If a chest is opened, the map-local fact is "this container is open" or "these items were removed"; the text shown to the player comes from a key plus arguments at presentation time.

Gameplay code should prefer returning a small result shape:

```fsharp
type LocalizedMessage =
    { Key: string
      Args: IReadOnlyDictionary<string, LocalizedArg> }
```

`LocalizedArg` is a closed union, not an untyped argument bag. It supports typed text, integers, 64-bit integers, decimals, floats, booleans, and date-time values. The exact outer message type can change, but the rule should hold: engine and game systems report facts and keys; the frontend or UI-facing game layer renders language-specific text.

Useful key namespaces:

```text
ui.*
tile.*
item.*
actor.*
fixture.*
interaction.*
error.*
map.*
```

These are conventions, not hard runtime rules. The important part is that keys are stable content identifiers. Renaming a key is a content migration.

## Frontend Consumption

Godot should not need to understand map internals to localize text. It should receive keys from engine-facing APIs and call the active localizer.

For example:

```text
player looks at rock
engine returns item.rock.desc
frontend resolves item.rock.desc through Localizer
frontend displays final string
```

For formatted messages:

```text
player takes 3 rocks
engine/game returns interaction.take_item with count=3 and itemNameKey=item.rocks.name
frontend resolves item.rocks.name, then calls Plural or Format depending on the message contract
```

The engine can expose helper APIs that bundle keys and typed args, but it should avoid embedding UI presentation choices in map, layer, visibility, serialization, or constructor code. This keeps localization reusable if the frontend changes.

## Serialization

Localization packs use a custom binary format with the magic header `AGLB1`. The binary writer stores the locale, key count, key strings, message tags, pieces, variants, aliases, and placeholder format strings.

This format is separate from the FlatBuffers map and entity serializers. That separation is good. Localization content has different authoring and loading needs than maps. Map serialization should store localization keys as strings where content needs them; it should not inline localized text or pack localization data into every map.

Fallback locale names in `[meta]` are parsed into the authoring model, but fallback resolution is currently supplied explicitly when constructing a `Localizer`. That keeps runtime locale selection outside the pack.

## Tests

Localization correctness tests exist in `tests.fsx`, with focused regression coverage in `localization-tests.fsx`.

Parser coverage includes:

- simple messages
- placeholders and placeholder format strings
- plural entries
- select entries
- aliases
- triple-quoted multiline strings
- escape sequences
- metadata
- duplicate key rejection
- missing `other` rejection

Packer coverage includes:

- parse/build/binary round trip
- message shape preservation
- plural and select variant preservation
- alias preservation
- empty pack round trip
- zig-zag encoding for exact numeric labels

Runtime localizer coverage includes:

- `Get` and `TryGet`
- formatting with typed arguments
- plural selection and count injection
- select fallback to `other`
- alias chains
- alias cycle detection
- fallback packs
- binary-loaded localizers

The focused script currently pins the negative exact-variant binary round trip, because that path goes through the real packer encoder and decoder.

## Design Boundaries And Operating Rules

Localization is not a renderer and not a UI system. It is content lookup and message rendering data. The Godot layer still owns fonts, layout, rich text, wrapping, icons, input glyphs, accessibility choices, and screen-specific presentation.

Localization keys are not save data by themselves. Live saves should store game facts. If the fact needs text later, the game resolves a key from that fact or stores a stable content id that leads to a key.

Do not localize engine control flow. Door logic should not branch on the English string "Locked". It should branch on typed state and return a message key for presentation.

Do not make reusable definitions hold rendered text. A tile, item, NPC, or fixture definition should hold keys. The active locale may change without rebuilding map data.

Do not infer locale fallback from arbitrary global state. The caller should choose the primary pack and fallback packs explicitly.

## Future Work

Separate plural classification from the selector-name heuristic. Today `count` means `Plural`; any other selector means `Select`. A more explicit authoring shape would be clearer and safer for future non-count plural selectors.

Consider CLDR plural rules when the game needs real multi-language plural behavior. The current runtime is intentionally small and English-like: exact labels win first, then `zero`, `one`, or `other`.

Add key coverage validation across locale packs. Tooling should report missing keys, extra keys, duplicate aliases, alias cycles, missing placeholders, and placeholder mismatches between locales before content reaches runtime.

Add binary format version details beyond the `AGLB1` magic if the format starts changing. The current header is enough for the first pack format, but future migrations should be deliberate.

Pre-resolve or cache aliases if localization lookup becomes hot. The current iterative, cycle-safe alias resolution is robust and simple, but it allocates a visited set during resolution.

Reduce per-call allocation in plural formatting if it appears in hot UI paths. `Plural` currently overlays the `count` argument by creating a small dictionary. That is fine for normal UI text, but there is a clear optimization path if needed.

Consider arrays for runtime pieces if profiling says list traversal matters. The current representation is simple and already packed into arrays at the message level; changing `SimpleMessage` is only worth it with evidence.

Add editor tooling around `.agl` authoring. The useful first tools are parse diagnostics with line/column context, key search, stale-key detection against engine content definitions, and binary pack generation.

## Historical Notes

### 2026-07-07

- Replaced boxed localization arguments with the typed `LocalizedArg` union and changed the public runtime `Args` alias to `IReadOnlyDictionary<string, LocalizedArg>`.
- Updated `Format`, `Select`, and `Plural` to render typed arguments without `obj`/`box` and to use the localizer's configured `formatProvider` consistently.
- Updated localization tests and docs to use typed arguments, including plural count injection as `LocalizedArg.Int64`.

### 2026-07-06

- Added the first live localization system document.
- Recorded the existing `.agl` authoring syntax, parser model, `AGLB1` binary pack, runtime localizer, fallback behavior, and test coverage.
- Established the ownership rule: engine data stores keys and typed facts; the presentation layer renders localized strings.
- Fixed `AGLB1` zig-zag encoding for negative exact variant labels and added focused regression coverage.
- Recorded future work for locale-sensitive formatting correctness, explicit plural authoring, CLDR plural behavior, key coverage tooling, and pack version hardening.
