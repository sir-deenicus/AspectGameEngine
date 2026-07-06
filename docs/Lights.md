# Lights

This document explains the intended engine-side contract for 2D lights: authored map lights, sprite-attached lights, spell-created lights, and time-linked environmental lights such as sun, moon, and window beams.

The short version: lights are gameplay/content data plus renderer-facing hints. The engine should know what emits light, where the light is anchored, what state controls it, and what compact profile the frontend should use. The engine should not own Godot light nodes, textures, render resources, or per-frame light drawing.

## Current Behavior

There is no general light-source system in the engine yet.

The current light-shaped behavior lives in `SpellsCore.fs`. `SpellCompiler.tryCompile` recognizes `VerbGlyph.Conjure` plus `NounGlyph.Light` and emits a `SpellExecution` with a `VisualHint.LightOrb`. The hint carries:

- color
- abstract radius
- intensity
- lifetime
- target point

That is a useful boundary: spell compilation can produce engine-agnostic execution data, and the frontend can decide how to present it. It is not yet a full light model. It does not attach to map placements, serialize as authored map content, interact with time of day, or expose a shared light profile shape.

The rest of the current engine uses light-adjacent concepts, but they are separate:

- `TileOpacity` controls FOV and occlusion semantics.
- `FOV.fs` computes visibility and exploration.
- `ChunkOcclusionManager.fs` builds tile-space opacity rectangles.
- `docs/Sprite-Shadows.md` covers sprite lighting shadow shapes for Godot light occluders.
- `docs/Animation.md` covers future frame/cycle data that may eventually change a light or shadow profile per frame.

## Target Model

Lights should use the same broad split as sprites, interactions, and shadows:

- Definition: reusable data that describes what kind of light this is.
- Placement: one authored occurrence on a map, or one occurrence attached to a map object.
- State: mutable live facts such as enabled, flicker seed, current intensity multiplier, remaining lifetime, or time-linked phase.
- Frontend profile: compact renderer-facing hints such as color, shape, extent, masks, shadow behavior, and texture key.

Definitions can live in tileset data, entity registry data, spell definitions, or a future light-profile registry. Placements belong to maps or live spell/effect execution. State belongs to the live game/map state or save stream. Godot nodes and resources belong to the frontend.

The engine can say "this torch fixture emits warm light with profile `torch.small` at this fixture's current position." The frontend decides whether that becomes a `PointLight2D`, a pooled RenderingServer light, a shader uniform, a baked glow layer, or a custom renderer path.

## Light Anchors

A light source needs an anchor.

The useful anchor kinds are:

```fsharp
type LightAnchor =
    | MapCell of position:GridPos
    | MapPoint of x:float32 * y:float32
    | LayerObject of localObjectId:int
    | LayerSlot of position:GridPos * slot:InteractionSlot
    | SpellInstance of instanceId:System.Guid
    | Environment
```

The exact names can change, but the rule should hold: anchoring is explicit. The renderer should not guess where a light belongs by scanning texture names or tile art.

`MapCell` is for lights authored directly on the map: a window beam, a glowing rune, a brazier position, a lava vent, or a fixed magical lamp.

`MapPoint` is for lights that need sub-tile placement.

`LayerObject` is the preferred long-term anchor for sprite-attached lights. If a lantern fixture, actor, carried orb, fire item, or glowing decal moves, the light follows the map-local object identity.

`LayerSlot` is a practical bridge before all layer placements have local object ids. It can point at a cell plus a specific slot such as fixture, actor, item, or decal. This is less robust than local identity when objects move or stacks change.

`SpellInstance` is for temporary lights created by spell execution.

`Environment` is for map-wide or screen-wide light, such as sunlight, moonlight, ambient night tint, or storm flashes.

## Tile And Sprite Attached Lights

Lights may be attached to a tile position or to a sprite/entity placement.

A tile-attached light is a map placement. It should not require the base tile to become a renderer object. For example, a light source can be authored at cell `(10, 4)` and use a `window.sunbeam` profile. If the tile later changes from one floor sprite to another, the light remains a map light unless the editor explicitly removes it.

A sprite-attached light follows the sprite placement. If a torch fixture moves, the torch's light moves. If an actor carries a lantern, the lantern light follows the actor. If a spell creates a light orb actor or effect object, its light follows that live object.

Reusable entity definitions may carry default light definitions. For example, every fixture with entity id `brazier.lit` may default to a warm flickering light. But live state still belongs to the placement. Turning off one brazier must not turn off every brazier that shares the same registry definition.

For base tiles that are intrinsically luminous, such as lava or magical floor tiles, tile properties may eventually carry a default light definition. That should be treated as a content default, not as proof that all tiles are renderer nodes. If per-cell state matters, promote the light to an explicit map placement or map-local state entry.

## Light Profiles

The engine should describe light profiles in frontend-neutral terms, with enough Godot-facing detail that the adapter does not need to guess.

A compact shape could look like this:

```fsharp
type LightKind =
    | Point
    | Directional
    | Area
    | Texture

type LightFalloff =
    | Smooth
    | Linear
    | Hard
    | TextureDriven

type LightProfile =
    { Key: string
      Kind: LightKind
      Color: ColorRgba
      Intensity: float32
      Radius: float32
      Width: float32
      Height: float32
      RotationDegrees: float32
      TextureKey: string option
      Falloff: LightFalloff
      CastsShadows: bool
      LightMask: int
      ShadowMask: int
      BlendMode: string
      HeightHint: float32
      Flicker: FlickerProfile option }
```

`Key` is stable content vocabulary: `"torch.small"`, `"brazier.large"`, `"mage_light.prime"`, `"window.sunbeam"`, `"moon.window"`, `"lava.glow"`.

`Kind` tells the frontend which family of light to use. Godot's current 2D light model has a common `Light2D` base, `PointLight2D` for positional texture-shaped lights, and `DirectionalLight2D` for distant parallel light such as sun or moon. The engine should not expose those exact node types as its own domain model, but the profile should map cleanly to them.

`Radius`, `Width`, `Height`, `RotationDegrees`, and `TextureKey` describe extent and shape. In Godot terms, a point light's visible shape often comes from a texture and scale, while directional light ignores position and uses direction. The profile should keep those ideas explicit without requiring the engine to load the texture.

`LightMask` and `ShadowMask` are frontend-facing masks. They allow some lights to affect only certain canvas items and some sprite shadow casters to affect only selected lights.

`HeightHint` is for normal-mapped lighting or renderer-specific height behavior. It is a hint, not gameplay height.

`Flicker` is data for procedural variation. A torch, candle, fire, unstable spell, or machinery light should not require Godot to hardcode a behavior per asset name.

## Time-Linked Lights

Sun, moon, and window light should be modeled as special time-linked light sources, not as implicit map rules.

There should not be a hardcoded "indoor map admits outdoor light" flag. There should not be a special "window tile admits light" rule. A window is just art unless content attaches a time-linked light source to it.

That means:

- an outdoor map can have an environment light controlled by time of day
- an indoor map can have no outdoor light at all
- a room with a window can have a `window.sunbeam` or `window.moonbeam` light placement
- a cave entrance can have a placed daylight spill
- moon phase can affect any moon-linked light source

The engine needs world time data only if lighting, schedules, spells, encounters, or other game rules depend on it. A minimal future shape could be:

```fsharp
type MoonPhase =
    | New
    | WaxingCrescent
    | FirstQuarter
    | WaxingGibbous
    | Full
    | WaningGibbous
    | LastQuarter
    | WaningCrescent

type WorldLightTime =
    { MinuteOfDay: int
      MoonPhase: MoonPhase
      WeatherKey: string option }

type TimeLightCurve =
    { Key: string
      DayColor: ColorRgba
      NightColor: ColorRgba
      DawnColor: ColorRgba
      DuskColor: ColorRgba
      DayIntensity: float32
      NightIntensity: float32
      MoonPhaseScale: float32[] }
```

The exact curve representation can change. The important rule is that time-linked lights are authored light sources whose output is evaluated against world time. They are not inferred from tile type or map type.

## Godot Frontend Handoff

The frontend should consume resolved light instances from the engine and turn them into renderer resources.

A resolved light instance should include:

- stable id or source id
- anchor kind and current resolved position/direction
- profile key and evaluated profile values
- enabled state
- lifetime or removal hint for temporary lights
- masks and shadow settings
- optional event/change version for frontend pooling

For Godot, the likely mapping is:

```text
Point/Texture light -> PointLight2D or pooled equivalent
Directional light -> DirectionalLight2D or global shader/CanvasModulate path
Color -> Light2D color
Intensity -> Light2D energy
TextureKey -> PointLight2D texture
Radius/extent -> texture scale, generated texture size, or adapter scale
HeightHint -> Light2D/PointLight2D height where useful
LightMask -> item cull mask or equivalent renderer filtering
ShadowMask -> shadow cull mask / occluder mask pairing
CastsShadows -> shadow-enabled frontend light
```

Godot's class reference describes `Light2D` as color, energy, blend/range, and shadow-related parameters; `PointLight2D` as a positional light whose shape is defined by a texture; and `DirectionalLight2D` as a distant parallel-ray light suitable for sun or moon. The engine profile should be close enough to those concepts to make the adapter boring.

The frontend remains free to choose a different renderer path. For performance, it may pool light nodes, use RenderingServer resources, batch data into shaders, cull lights by camera view, or collapse several ambient lights into one screen-wide effect.

## Relationship To Shadows

Lights and sprite shadow casters are separate systems that meet in the frontend.

`docs/Sprite-Shadows.md` says which sprite entities have local shadow-casting shapes. This document says which map or sprite objects emit light. A sprite can emit light without casting a shadow. A sprite can cast a shadow without emitting light. A base tile can block FOV without casting a renderer lighting shadow.

The engine should not decide renderer shadow geometry from `TileOpacity`. It can provide both:

- light instances
- sprite shadow caster metadata

Godot then pairs lights and occluders through masks, visibility, pooling, and renderer state.

## Relationship To Effects

Lights and visual effects are siblings, not the same system.

A fire may need both:

- a light source: orange flickering illumination
- an effect source: flame particles, smoke, sparks, heat shimmer

Both need anchors, lifetime, enabled state, and frontend profile keys. They should share conventions where useful, but they should not be collapsed into one type. A glowing crystal may emit light with no particles. A dust burst may be an effect with no light. A spell impact may briefly emit both.

Light is optional for an effect source, and a particle effect may depend on that optional linked light. For example, a fire particle profile can use the linked light's color and intensity as presentation inputs. If no linked light exists, the particle profile uses its own defaults.

`docs/Effects.md` covers particles, one-shot effects, looping effects, fire/smoke/sparks, effect anchoring, effect lifetimes, optional linked lights, and frontend handoff.

## Serialization

There are three persistence concerns.

Reusable light defaults should serialize with the content stream that owns the definition:

- tile light defaults with tileset data, if tile-default lights are added
- sprite/entity light defaults with entity registry data
- spell light defaults with spell/content definitions, when those become serialized

Map-authored light placements should serialize with map content or a map sidecar. A fixed window beam, placed magical lamp, or map environment light belongs to the map, not to the global registry.

Live light state belongs to game/save state. If a torch has been extinguished, a lamp has been moved, a spell light has 2.4 seconds left, or a weather event is changing sunlight intensity, that is runtime progress and should not be written back into ordinary map content.

As with interactions, FlatBuffers are the likely runtime/interchange format. Avoid reflection-based JSON or a custom text parser. If authoring needs a pleasant text surface, prefer editor UI or F# builder scripts that emit schema-backed binary data.

## Design Boundaries And Operating Rules

The engine is not a renderer. For lights, that is a strength: the engine can keep a stable, testable contract while Godot rendering evolves from nodes to pooled resources or shaders.

Do not infer lights from terrain art. A window tile does not automatically admit sunlight. An indoor map does not automatically receive or block outdoor light. Content should place explicit time-linked light sources where it wants sun, moon, or window illumination.

Do not make light authoritative for FOV or exploration. FOV determines what can be known or seen according to gameplay visibility. Renderer lighting makes the scene look lit, dim, colored, or shadowed.

Do not make `TileOpacity` a lighting field. Opacity belongs to visibility and occlusion. Light profiles belong to light sources.

Do not store live per-placement light state on reusable registry definitions. Turning off one torch should not change every torch with the same sprite id.

Sprite-attached lights should follow map-local placement identity when possible. Coordinate-and-slot anchors are acceptable as an early bridge, but they are weaker for moved objects and stacked layer content.

Time-linked environmental light is authored light, not a map category rule. Sun, moon, and window beams should be explicit light sources evaluated against world time and moon phase.

## Future Work

Add core light types: `LightAnchor`, `LightProfile`, `LightSource`, resolved light instances, flicker profiles, and time-linked profile curves.

Decide where authored light defaults live for tile properties, entity registry entries, spell definitions, and map placements.

Add map-local light placement/state support so individual lamps, torches, windows, spell lights, and magical objects can be enabled, disabled, moved, or saved independently.

Add world time and moon phase primitives if lighting becomes the first engine feature that needs them. Keep the representation general enough for schedules, encounters, and future systems.

Convert `SpellsCore.VisualHint.LightOrb` into, or map it through, the shared light-source contract so mage light uses the same frontend handoff as authored lights.

Add a frontend-facing light query or change feed that returns resolved visible/relevant light instances without exposing Godot nodes to the engine.

Add FlatBuffers schema and serializers for light profiles and map light placements once the first implementation shape is chosen.

Add tests for profile evaluation, time-linked intensity/color, sprite-attached light movement, map-local state isolation, serialization round trips, and spell-created light lifetimes.

Bridge light sources and visual effect sources with an optional shared-source or linked-id model so particle profiles can use linked light color, intensity, or radius without requiring every effect to own a light.

## Source Map

- `SpellsCore.fs` - current `Conjure + Light` spell compilation and `VisualHint.LightOrb`.
- `Types.fs` - `GridPos`, `GridDelta`, and future likely home for shared light primitives if they stay small.
- `LayerGrid.fs` - current sprite/entity registry model and future home for sprite default light metadata.
- `Maps.fs` - runtime map state and future home for map-local light placements if they are stored directly on maps.
- `MapEditor.fs` - future editor placement and conversion rules for map-authored lights.
- `MapTypeSerializer.fs` - future serializer location for map light placements if lights become part of map serialization.
- `EntityRegistrySerializer.fs` - future serializer location for sprite/entity default light metadata.
- `FlatBufferTypes/Schemas` - future schema location for light profiles, placements, and runtime/interchange data.
- `docs/Sprite-Shadows.md` - sprite lighting shadow shapes, separate from light emitters.
- `docs/Animation.md` - future animation cycles that may later vary light profiles per frame or state.
- `docs/Interactions.md` - interaction effects may enable, disable, spawn, remove, or modify lights.
- `docs/Effects.md` - visual-effects contract for particles, fire, smoke, sparks, optional linked lights, and similar frontend effects.

## Historical Notes

### 2026-07-05

- Added the first live lights document.
- Recorded that the current engine has only a spell light hint, not a general light-source system.
- Set the design direction that lights can be anchored to map positions, sprite placements, spell instances, or environment slots.
- Clarified that sprite-attached lights should move with their map-local object placement.
- Removed the idea of special indoor/window light-admission rules. Sun, moon, and window illumination should be explicit time-linked light sources authored on the map.
- Captured Godot-facing profile needs while preserving the engine/frontend split.
- Preserved `docs/Effects.md` as a pending documentation task for particles and other visual effects.
