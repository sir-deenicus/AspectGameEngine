# Effects

This document explains the intended engine-side contract for frontend visual effects: particles, smoke, steam, sparks, fire, magical fields, spell impacts, persistent spell visuals, and other renderer-side presentation.

The short version: effects are presentation data, not gameplay authority. The engine should say which effect profile exists, where it is anchored, how long it lives, whether it is enabled, and which optional light source belongs with it. The frontend owns particle simulation, renderer nodes, shaders, textures, pooling, and the concrete meaning of each profile key.

## Current Behavior

There is no general visual-effects system in the engine yet.

The closest existing hook is in `SpellsCore.fs`. `SpellCompiler.tryCompile` can return a `SpellExecution` with `VisualHint.LightOrb` for mage light. That is a light hint rather than a particle/effect contract, but it shows the right general direction: spell or gameplay code can return frontend-neutral presentation instructions without creating renderer objects.

Current related systems are separate:

- `docs/Lights.md` covers light sources, anchors, time-linked light, and Godot handoff.
- `docs/Sprite-Shadows.md` covers sprite lighting-shadow shapes.
- `docs/Animation.md` covers sprite frame and cycle data.
- `docs/Interactions.md` covers interaction effects in the gameplay/data sense.

This document uses "effect" to mean visual presentation effects. It does not mean gameplay effect lists such as unlocking a door, granting a key, or damaging an actor.

## Target Model

Effects should follow the same ownership rule as lights: the engine owns a minimal data contract, and the frontend owns rendering.

A visual source may have:

- a particle/effect profile
- an optional light source
- a shared anchor
- a shared lifetime
- shared enabled/disabled state
- shared instance identity or seed

That lets common visuals stay together without collapsing different concepts into one blob. A fire can have particles plus a light. Steam can have particles and no light. Mage light can be a light with optional motes. A magical wall can have particles plus colored light while the separate gameplay system decides whether the wall blocks movement or damages actors.

The important design decision: light is optional, and a particle effect may depend on that optional light.

For example, a particle profile can say "tint flame particles from the linked light color" or "scale ember brightness from linked light intensity." If no linked light exists, the particle profile uses its own defaults. This keeps fire, spells, and magical fields coherent while still allowing particles-only effects.

## Effect Anchors

Effects need explicit anchors. The renderer should not infer effect location from art names, tile types, or spell ids.

A useful shape mirrors lights:

```fsharp
type EffectAnchor =
    | MapCell of position:GridPos
    | MapPoint of x:float32 * y:float32
    | LayerObject of localObjectId:int
    | LayerSlot of position:GridPos * slot:InteractionSlot
    | SpellInstance of instanceId:System.Guid
    | Environment
```

`MapCell` works for authored effects such as a steam vent, waterfall mist, fire pit, or glowing floor effect.

`MapPoint` works for sub-tile placement.

`LayerObject` is the preferred long-term anchor for sprite-attached effects. A carried torch, moved barrel with smoke, burning actor, or floating spell object should move its effect with the object.

`LayerSlot` is an early bridge before all map placements have local object ids. It is weaker than local identity for moved objects and stacked layer content.

`SpellInstance` works for temporary spell visuals.

`Environment` works for map-wide or camera-wide presentation such as rain, snow, drifting dust, ash, or ambient magical motes.

## Lifetime And State

Effects need a small lifecycle model:

```fsharp
type EffectLifetime =
    | OneShot
    | Timed of seconds:float32
    | Looping
    | PersistentUntilRemoved
```

`OneShot` is for bursts: spell impact, dust puff, hit spark, extinguish puff.

`Timed` is for temporary loops or decays: smoke for five seconds, lingering spell motes, a short flame jet.

`Looping` is for ordinary repeating presentation while the source exists: torch flame, steam vent, brazier fire.

`PersistentUntilRemoved` is for map or gameplay-owned visuals whose lifetime is controlled externally: magical walls, persistent fields, environmental emitters, active hazards.

The engine state should stay compact:

- enabled
- lifetime
- age or remaining time when needed for save/runtime state
- seed when deterministic variation matters
- scalar overrides such as scale or intensity

The engine should not store individual particles.

## Minimal Effect Source

A compact future shape could be:

```fsharp
type EffectLightBinding =
    { UseLightColor: bool
      UseLightIntensity: bool
      UseLightRadius: bool }

type ParticleEffectRef =
    { ProfileKey: string
      LightBinding: EffectLightBinding option }

type VisualEffectSource =
    { Id: int
      Anchor: EffectAnchor
      Lifetime: EffectLifetime
      Enabled: bool
      Seed: int option
      OffsetX: float32
      OffsetY: float32
      RotationDegrees: float32
      RenderLayer: int
      Scale: float32
      Intensity: float32
      ParticleEffect: ParticleEffectRef option
      Light: LightSource option }
```

The exact names can change. The rule should hold:

- particles are optional
- light is optional
- both can share the same anchor and lifetime
- particles may bind to the optional light's color, intensity, or radius
- either side can exist without the other

This is enough for the engine to place and control presentation without knowing Godot particle internals.

## Profile Keys And Overrides

The effect profile key is the main authoring handle.

Examples:

```text
fire.small
fire.wall
smoke.thin
steam.pipe
spark.impact
spell.impact.arcane
spell.field.poison
spell.wall.flame
ambient.ash
ambient.dust
```

The frontend owns what those keys mean. A profile may map to `GPUParticles2D`, `CPUParticles2D`, an animated sprite, a shader, a pooled scene, a custom draw path, or a bundle of several renderer emitters.

The engine should expose only broad per-instance overrides:

- `Scale`: larger or smaller instance.
- `Intensity`: denser, brighter, or quieter instance.
- `RotationDegrees`: orientation for vents, sprays, walls, beams, and directional bursts.
- `Seed`: deterministic variation.
- `RenderLayer`: draw-order hint.
- `OffsetX/Y`: local origin correction.

Avoid making the engine author every low-level particle knob. Emission rate, particle lifetime, velocity curves, color ramps, gravity, turbulence, sprite sheet animation, trails, and sub-emitters are frontend profile details unless a future gameplay system genuinely needs to inspect them.

## Optional Light Dependency

Many effects co-occur with lights, but the light should remain optional.

A useful authoring pattern is:

```text
VisualEffectSource
  ParticleEffect = "fire.small"
  Light = "fire.small.warm"
  ParticleEffect.LightBinding = use color + intensity
```

The particle profile can then use the linked light as a shared presentation driver:

- flame particle color follows light color
- ember brightness follows light intensity
- glow sprite scale follows light radius
- magical motes inherit a spell light tint

If the same particle effect is authored without a light:

```text
VisualEffectSource
  ParticleEffect = "smoke.thin"
  Light = none
```

the frontend uses the particle profile's own defaults.

This keeps authoring flexible without forcing every effect to own a light. Smoke, steam, dust, snow, sparks, and many impact puffs do not need light. Fire, mage light, lava glow, magical walls, lightning impacts, and persistent spell fields often do.

## Relationship To Gameplay

Particle effects do not know gameplay.

A magical wall may block movement, damage actors, emit particles, and cast colored light. Those are separate contracts:

- gameplay area or interaction state decides blocking, damage, duration, dispel rules, and save semantics
- particle effect source presents visual motion
- light source presents illumination

If smoke blocks vision, that is not because the smoke particle effect exists. The gameplay/map system must add an opacity, FOV, or area-effect rule, and the smoke effect simply presents it.

If fire damages actors, that is not because the fire particle profile exists. The gameplay system owns the hazard. The particle effect is just the presentation of that hazard.

This separation keeps the effect renderer replaceable and prevents art changes from changing rules.

## Godot Frontend Handoff

The Godot layer should consume resolved visual effect sources and instantiate or update renderer resources.

The likely Godot mapping is:

```text
ParticleEffect.ProfileKey -> packed scene, GPUParticles2D, CPUParticles2D, shader effect, or custom pooled renderer
Anchor -> node/global position, canvas transform, or pooled draw origin
Lifetime -> one_shot/emitting/timer/removal policy
Seed -> fixed particle seed or renderer random seed
Scale -> node scale, process material scale, or profile multiplier
Intensity -> amount ratio, alpha multiplier, color strength, or profile multiplier
RotationDegrees -> node rotation or material direction
RenderLayer -> canvas item layer/z index/sorting bucket
Light -> optional linked light instance from docs/Lights.md
LightBinding -> frontend uses linked light values when updating particle material/profile
```

Godot exposes both GPU and CPU 2D particle nodes, and its particle process material covers concepts such as emission shape, particle lifetime behavior, velocity, gravity, color/alpha curves, trails, collisions, and sub-emitters. Those details are exactly why the engine should mostly pass profile keys and broad overrides rather than clone the whole renderer API.

The frontend may also use pooled nodes or RenderingServer paths for performance. The engine contract should not care.

## Serialization

Reusable effect profiles belong to frontend content or a future shared content registry, not ordinary runtime map state.

Map-authored effect placements should serialize with map content or a map sidecar. A steam vent, ambient ash field, magical floor shimmer, or placed fire source is authored map content.

Live effect state belongs to game/save state. Remaining lifetime, enabled state, moved anchors, deterministic seed, and runtime-spawned spell effects are live progress data.

If effect placements need to be consumed by the engine at runtime, FlatBuffers are the likely interchange format. Avoid reflection-based JSON and avoid inventing a custom text parser. As with interactions and lights, authoring can come from editor UI or F# builder scripts that emit schema-backed binary data.

## Design Boundaries And Operating Rules

Effects are presentation data, not gameplay authority.

Do not infer damage, movement blocking, opacity, FOV blocking, inventory behavior, or interaction behavior from an effect profile key.

Do not store individual particles in the engine. Store effect sources, profile keys, anchors, lifecycle, optional light, and small scalar overrides.

Do not require every particle effect to have a light. Light is optional.

Do allow particle effects to depend on an optional linked light for presentation values such as color, intensity, or radius.

Do not make the engine mirror the full Godot particle API. Most particle knobs belong in frontend profiles.

Sprite-attached effects should follow map-local placement identity when possible. Coordinate-and-slot anchors are an acceptable early bridge, but they are weaker for moved objects and stacked layer content.

## Future Work

Add core effect types: `EffectAnchor`, `EffectLifetime`, `ParticleEffectRef`, optional light binding, and `VisualEffectSource`.

Decide whether light/effect sources share a common presentation-source container or remain separate sources linked by id.

Add map-authored effect placements for simple static emitters such as steam vents, ambient motes, fire pits, and magical fields.

Add runtime effect spawning for spells and interactions.

Add a frontend-facing effect query or change feed that returns resolved effect sources without exposing Godot nodes to the engine.

Add FlatBuffers schema and serializers for map effect placements once the first implementation shape is chosen.

Add save/runtime support for enabled state, remaining lifetime, deterministic seed, and runtime-spawned persistent effects.

Add tests for source resolution, sprite-attached movement, optional light binding, lifetime expiration, map-local state isolation, and serialization round trips.

## Source Map

- `SpellsCore.fs` - current spell execution and `VisualHint.LightOrb`, likely future bridge into shared presentation/light/effect sources.
- `Types.fs` - `GridPos`, `GridDelta`, and future likely home for small shared effect primitives.
- `LayerGrid.fs` - current entity registry and future source for sprite-attached default effect metadata.
- `Maps.fs` - runtime map state and future home for map-authored effect placements if stored directly on maps.
- `MapEditor.fs` - future editor placement and conversion rules for map-authored effects.
- `MapTypeSerializer.fs` - future serializer location for map effect placements if effects become part of map serialization.
- `EntityRegistrySerializer.fs` - future serializer location for sprite/entity default effect metadata.
- `FlatBufferTypes/Schemas` - future schema location for effect placements and runtime/interchange data.
- `docs/Lights.md` - light sources that can optionally accompany and drive particle effects.
- `docs/Animation.md` - future animation cycles that may drive sprite-like effects.
- `docs/Interactions.md` - interaction gameplay effects may spawn, enable, disable, or remove visual effects.

## Historical Notes

### 2026-07-05

- Added the first live effects document.
- Set the design direction that particle effects are presentation sources with profile keys, anchors, lifetimes, seeds, and scalar overrides.
- Recorded that light is optional, and particle effects may depend on linked light values for presentation.
- Clarified that gameplay areas, particle emitters, and light sources can co-occur but should not collapse into one gameplay/rendering blob.
- Recorded that detailed particle authoring belongs primarily in frontend profiles, while the engine carries only the minimal renderer-facing contract.
