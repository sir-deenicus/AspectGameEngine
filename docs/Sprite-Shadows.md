# Sprite Lighting Shadows

This document explains the intended engine-side contract for Godot 2D lighting shadows cast by sprites. It is about renderer lighting shadows, not FOV visibility shadows and not tile opacity.

The short version: base tiles do not cast lighting shadows. Lighting shadow shapes belong only to layered sprite entities: actors, fixtures, items, decals, and future layer sprite types. If an environmental object needs to cast a visual shadow, model it as a fixture or another layer sprite, not as a base tile. The engine should store frontend-neutral shadow-shape metadata; the Godot frontend should turn that metadata into `LightOccluder2D`/`OccluderPolygon2D` nodes, RenderingServer occluder RIDs, or another cached renderer representation.

## Explanation

Godot 2D lighting shadows are driven by occluder polygons. A sprite does not cast a light shadow from its visible pixels just because it is drawn. The renderer needs a shadow shape: usually an `OccluderPolygon2D` assigned to a `LightOccluder2D`, or the equivalent lower-level RenderingServer occluder resources.

This engine should not own those Godot nodes. It should own stable data that says whether a layered sprite casts a lighting shadow, and what local polygon shape should be used.

The important split is:

- `TileOpacity` controls FOV, exploration, and map occlusion semantics.
- `ChunkOcclusionManager` produces tile-space occluder rectangles for map opacity/visibility-style geometry.
- sprite lighting shadows describe visual silhouettes for layered sprite entities.

Those are related concepts, but they should not collapse into one field. A fixture might block FOV and cast a sprite-shaped shadow. Another sprite might cast a visual shadow but not block movement or FOV. A wall tile may block FOV but still not participate in sprite lighting shadows because base tiles are excluded by design.

## Tile Rule

Base tiles do not cast renderer lighting shadows.

That rule keeps the tile layer cheap, stable, and independent from Godot lighting setup. Tile cells remain map terrain and FOV inputs. If a tree, pillar, sign, wall ornament, statue, tall grass clump, brazier, crate, or other authored object should cast a visual lighting shadow, represent it as a layer sprite, usually a fixture.

This avoids per-tile shadow metadata in `TileProperties` and keeps map cells from turning into renderer objects. It also gives the editor a clear content rule: terrain is terrain; shadow-casting objects are placed entities.

## Proposed Engine Data Shape

A compact future shape should be enough:

```fsharp
type ShadowShape =
    | NoShadow
    | LocalPolygon of (int * int)[]
    | LocalPolygons of (int * int)[][]

type ShadowCaster =
    { Shape: ShadowShape
      Closed: bool
      LightMask: int
      SdfCollision: bool
      OffsetX: int
      OffsetY: int }
```

`LocalPolygon` and `LocalPolygons` are local sprite-space coordinates. They are not map coordinates and not Godot node positions. The frontend applies sprite position, anchor, draw offset, scale, facing, and any render transform.

`LocalPolygons` is necessary because alpha extraction can produce disconnected opaque islands. Do not silently keep only the first polygon unless the asset is known to have one connected silhouette.

`Closed` maps to Godot's closed occluder polygon behavior. Closed polygons block light from any direction. Open polygons are edge-like and should be used only when the content deliberately wants one-sided outline occlusion.

`LightMask` maps to the frontend light/occluder mask. It lets a sprite cast shadows only for selected lights.

`SdfCollision` maps to Godot's SDF collision participation for custom shader use. Keep it explicit because not every shadow caster needs SDF.

`OffsetX` and `OffsetY` provide a stable origin correction between the engine's sprite anchor and the polygon's authored local coordinate space.

## Where It Should Live

Do not add this to `TileProperties`.

The simplest engine placement is `SpriteProperties`:

```fsharp
type SpriteProperties =
  { Sprite: SpriteRef
    SpriteType: SpriteType
    RenderLayer: int
    ShadowCaster: ShadowCaster option }
```

That makes shadow data available for every layered sprite type without repeating fields.

A more explicit alternative is to add `ShadowCaster option` to each type-specific record:

- `ActorProperties`
- `FixtureProperties`
- `ItemProperties`
- `DecalProperties`

That lets defaults vary by kind, but it duplicates the same field. For the current engine shape, `SpriteProperties` is probably the cleaner first pass.

## Godot Frontend Handoff

The Godot side can consume `ShadowCaster` in two ways.

For node-based rendering, create or pool a `LightOccluder2D` for each visible shadow-casting sprite and assign an `OccluderPolygon2D`. The `ShadowCaster` fields map directly:

```text
ShadowShape.LocalPolygon -> OccluderPolygon2D.Polygon
ShadowShape.LocalPolygons -> multiple polygons/occluders
Closed -> OccluderPolygon2D.Closed
LightMask -> LightOccluder2D.OccluderLightMask
SdfCollision -> LightOccluder2D.SdfCollision
OffsetX/Y -> local transform or point offset
```

For lower-level pooled rendering, use RenderingServer occluder resources. The frontend can create occluder polygon RIDs, set their shape, and attach them to canvas light occluders without representing every sprite as a normal scene node. This fits the frontend todo note: the renderer is already avoiding individual nodes for most sprites, so a pooled/RID path may be better than child `LightOccluder2D` nodes.

## Generating Shapes From Sprites

The frontend todo proposes generating occluder polygons from texture alpha:

```text
texture image -> alpha bitmap -> opaque polygons -> occluder polygon
```

That approach is acceptable as an editor/import-time or cache-on-first-use generator. It should not run per sprite per frame.

Recommended ownership:

- Generate polygons in the frontend asset pipeline, editor tooling, or a cache keyed by sprite asset/frame.
- Store approved polygons back into engine registry data once authored, if the engine should serialize them.
- Reuse generated polygons for all instances of the same sprite/frame.
- Regenerate only when the source sprite image, alpha threshold, simplification epsilon, or shadow-authoring settings change.

Important details:

- Preserve multiple polygons when the silhouette has disconnected parts.
- Apply sprite centering/anchor offsets once and store the result consistently.
- Choose an epsilon that avoids noisy pixel stair-steps without erasing important silhouettes.
- Treat alpha-generated shapes as a starting point. Hand-authored polygons may be better for gameplay readability and performance.

## Animation And Facing

Some sprites will have different silhouettes by facing or animation frame.

The first implementation can attach one `ShadowCaster` to the static `SpriteProperties`. That is enough for many fixtures, items, and decals.

A later implementation should allow animation cycles or frames to override the shadow shape when the silhouette changes. This belongs with the future framed animation-cycle work in `docs/Animation.md`.

Facing can be handled by the frontend when the shape is mirrorable. For asymmetric shapes, store separate shadow shapes per visual/facing/cycle rather than relying on a blind mirror.

## Serialization

Shadow-shape data should serialize with the entity registry stream, not the map stream.

Maps store layer placement by entity id. Entity registry data describes what that entity looks like and whether it casts a lighting shadow. This mirrors the existing `SpriteRef` and future animation-cycle ownership.

Adding this requires:

- F# types for `ShadowShape` and `ShadowCaster`.
- FlatBuffers schema entries under `entity_registry.fbs`.
- serializer/deserializer support in `EntityRegistrySerializer.fs`.
- round-trip tests in `entity-registry-test.fsx`.
- editor behavior for registry edits, especially undo/redo of sprite type or shadow-shape changes.

## Design Boundaries And Operating Rules

Do not put lighting shadow polygons on base tiles.

Do not infer lighting shadow behavior from `TileOpacity`. FOV blockers and renderer light occluders are different outputs.

Do not generate alpha polygons every frame. Generate once per asset/frame/settings combination, then cache or serialize.

Do not discard disconnected polygons by default. A sprite with multiple opaque islands needs multiple local polygons or multiple occluder resources.

Do not make lighting shadows authoritative for movement, occupancy, FOV, or map exploration. Shadow polygons are presentation data.

## Source Map

- `docs/2D-Engine.md` - layer sprite model and runtime/editor map split.
- `docs/Animation.md` - future per-frame animation data where frame-specific shadow silhouettes may later fit.
- `docs/Occluder-Task.md` - tile-space opacity rectangle occluder system, separate from sprite lighting shadows.
- `LayerGrid.fs` - current `SpriteProperties` and type-specific sprite property records.
- `EntityRegistrySerializer.fs` - future serializer location for shadow-shape data.
- `FlatBufferTypes/Schemas/entity_registry.fbs` - future schema location for shadow-shape data.
- `C:\Users\cybernetic\source\repos\aspectrpg\Design-docs\todo-list.md` - frontend todo note with the current Godot-side alpha-to-polygon sketch.

## Historical Notes

### 2026-07-05

- Added the first sprite lighting shadow document.
- Locked the content rule that base tiles do not cast renderer lighting shadows.
- Proposed frontend-neutral `ShadowShape` and `ShadowCaster` data for layered sprite entities.
- Recorded that alpha-to-polygon generation is acceptable as a cached frontend/import-time tool, not as per-frame runtime work.
- Recorded that shadow-shape persistence belongs in entity registry serialization, not map serialization.
