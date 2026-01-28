namespace AspectGameEngine.Spells

open System

// ----------------------
// Engine-agnostic primitives
// ----------------------

[<Struct>]
type Vec2 =
    { X: float32
      Y: float32 }

[<Struct>]
type ColorRgba =
    { R: float32
      G: float32
      B: float32
      A: float32 }

type NounGlyph =
    | Light = 0
    | Force = 1
    | Water = 2

type VerbGlyph =
    | Conjure = 0
    | Project = 1
    | Alter = 2
    | Bind = 3
    | Modulate = 4
    | Trick = 5
    | Influence = 6
    | Ward = 7
    | Decipher = 8
    | Displace = 9

type PotencyTier =
    | Tier1 = 1
    | Tier2 = 2
    | Tier3 = 3
    | Tier4 = 4
    | Tier5 = 5

type Polarity =
    | Neutral = 0
    | Positive = 1
    | Negative = -1

type RangeBand =
    | Self = 0
    | Touch = 1
    | Short = 2
    | Medium = 3
    | Long = 4
    | Extreme = 5

type Conveyance =
    | Caster = 0
    | Projectile = 1
    | Locus = 2
    | Touch = 3

type Shape =
    | Point = 0
    | Sphere = 1
    | Cone = 2
    | Wall = 3
    | Blade = 4
    | Ray = 5
    | Aura = 6

type DurationBand =
    | Instant = 0
    | Short = 1
    | Medium = 2
    | Long = 3
    | Sustained = 4
    | Permanent = 5

type Dynamics =
    | Static = 0
    | Flow = 1
    | Rotate = 2
    | Channel = 3

[<CLIMutable>]
type Contingency =
    { Anchor: string  // "Location" / "Object" (future: strong types)
      Trigger: string // "Timer" / "Proximity" / etc. 
    }

[<CLIMutable>]
type SpellAdverbs =
    { Potency: PotencyTier
      Polarity: Polarity
      Range: RangeBand
      Conveyance: Conveyance
      Shape: Shape
      Duration: DurationBand
      Dynamics: Dynamics
      Contingency: Contingency option }

module SpellAdverbs =
    let primeDefaults : SpellAdverbs =
        { Potency = PotencyTier.Tier1
          Polarity = Polarity.Neutral
          Range = RangeBand.Short
          Conveyance = Conveyance.Caster
          Shape = Shape.Point
          Duration = DurationBand.Short
          Dynamics = Dynamics.Static
          Contingency = None }

[<CLIMutable>]
type SpellSpec =
    { Id: string              // stable id: "spell.mage_light"
      Verb: VerbGlyph
      Noun: NounGlyph
      Adverbs: SpellAdverbs }

// ----------------------
// Numen, cost, casting outcomes
// ----------------------

[<CLIMutable>]
type NumenPool =
    { Current: int
      Max: int }

module NumenPool =
    let trySpend (amount: int) (pool: NumenPool) =
        if amount <= 0 then Ok pool
        elif pool.Current >= amount then Ok { pool with Current = pool.Current - amount }
        else Error "Insufficient Numen"

type CastOutcomeKind =
    | Succeeded = 0
    | Failed = 1
    | Backlash = 2

[<CLIMutable>]
type SpellTarget =
    { Point: Vec2 } // minimal for now; later: target actor id, tile coords, etc.

[<CLIMutable>]
type LightOrbHint =
    { Color: ColorRgba
      Radius: float32        // abstract “meters/tiles-ish”; adapter decides mapping
      Intensity: float32
    }

type VisualHint =
    | LightOrb of LightOrbHint

[<CLIMutable>]
type SpellExecution =
    { InstanceId: Guid
      SpecId: string
      Target: SpellTarget
      LifetimeSeconds: float32
      Visuals: VisualHint [] }

[<CLIMutable>]
type CastOutcome =
    { Kind: CastOutcomeKind
      Message: string
      CostNumen: int
      NewPool: NumenPool
      Execution: SpellExecution option }

// ----------------------
// Cost model (simple now, replaceable later)
// ----------------------

module CostModel =
    // This is intentionally simple but stable: the function boundary is what lasts.
    // Later we can replace internals with KL-based math without changing signatures.
    let estimateCostNumen (spec: SpellSpec) : int =
        let baseVerb =
            match spec.Verb with
            | VerbGlyph.Conjure -> 2
            | VerbGlyph.Project -> 2
            | _ -> 4

        let baseNoun =
            match spec.Noun with
            | NounGlyph.Light -> 1
            | NounGlyph.Force -> 2
            | NounGlyph.Water -> 2
            | _ -> 3

        let potencyMul =
            match spec.Adverbs.Potency with
            | PotencyTier.Tier1 -> 1.0
            | PotencyTier.Tier2 -> 1.5
            | PotencyTier.Tier3 -> 2.2
            | PotencyTier.Tier4 -> 3.5
            | PotencyTier.Tier5 -> 5.5
            | _ -> 1.0

        let durationMul =
            match spec.Adverbs.Duration with
            | DurationBand.Instant -> 0.8
            | DurationBand.Short -> 1.0
            | DurationBand.Medium -> 1.7
            | DurationBand.Long -> 2.8
            | DurationBand.Sustained -> 4.0
            | DurationBand.Permanent -> 10.0
            | _ -> 1.0

        let shapeMul =
            match spec.Adverbs.Shape with
            | Shape.Point -> 1.0
            | Shape.Aura -> 1.8
            | Shape.Sphere -> 2.0
            | Shape.Wall -> 2.5
            | _ -> 1.2

        let raw = float (baseVerb + baseNoun) * potencyMul * durationMul * shapeMul
        max 1 (int (MathF.Ceiling(float32 raw)))

// ----------------------
// Spell compilation (Spec -> engine-agnostic execution instructions)
// ----------------------

module SpellCompiler =
    let private lifetimeSeconds (duration: DurationBand) =
        match duration with
        | DurationBand.Instant -> 0.1f
        | DurationBand.Short -> 6.0f
        | DurationBand.Medium -> 18.0f
        | DurationBand.Long -> 45.0f
        | DurationBand.Sustained -> 9999.0f
        | DurationBand.Permanent -> 999999.0f
        | _ -> 6.0f

    let tryCompile (spec: SpellSpec) (target: SpellTarget) : Result<SpellExecution, string> =
        match spec.Verb, spec.Noun with
        | VerbGlyph.Conjure, NounGlyph.Light ->
            let life = lifetimeSeconds spec.Adverbs.Duration

            let orb =
                { Color = { R = 0.85f; G = 0.95f; B = 1.0f; A = 1.0f }
                  Radius = 5.0f
                  Intensity = 1.2f
                }

            Ok
                { InstanceId = Guid.NewGuid()
                  SpecId = spec.Id
                  Target = target
                  LifetimeSeconds = life
                  Visuals = [| VisualHint.LightOrb orb |] }

        | _ ->
            Error "Spell not implemented yet (compiler has no rule for this Verb+Noun)."

// ----------------------
// Casting API (this is what C# should call)
// ----------------------

module SpellApi =
    let tryCast (pool: NumenPool) (spec: SpellSpec) (target: SpellTarget) : CastOutcome =
        match SpellCompiler.tryCompile spec target with
        | Error msg ->
            { Kind = CastOutcomeKind.Failed
              Message = msg
              CostNumen = 0
              NewPool = pool
              Execution = None }

        | Ok exec ->
            let cost = CostModel.estimateCostNumen spec

            match NumenPool.trySpend cost pool with
            | Ok newPool ->
                { Kind = CastOutcomeKind.Succeeded
                  Message = "Cast succeeded."
                  CostNumen = cost
                  NewPool = newPool
                  Execution = Some exec }

            | Error _ ->
                // Minimal backlash placeholder: we still return Failed/Backlash boundary now,
                // but later we can implement tiering and random fields here.
                { Kind = CastOutcomeKind.Backlash
                  Message = "Overcast backlash (placeholder)."
                  CostNumen = cost
                  NewPool = pool
                  Execution = None }

    let mageLightPrime : SpellSpec =
        { Id = "spell.mage_light"
          Verb = VerbGlyph.Conjure
          Noun = NounGlyph.Light
          Adverbs =
            { SpellAdverbs.primeDefaults with
                Duration = DurationBand.Short
                Shape = Shape.Point
                Conveyance = Conveyance.Caster } }