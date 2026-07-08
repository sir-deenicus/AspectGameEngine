namespace AspectGameEngine

[<Struct>]
type SpriteLoc =
    val AtlasIndex: int
    val Row: int
    val Column: int
    new(atlasIndex, row, column) =
        { AtlasIndex = atlasIndex
          Row = row
          Column = column }

[<Struct>]
type GridPos =
    val X: int
    val Y: int
    new(x, y) = { X = x; Y = y }

[<Struct>]
type GridDelta = 
    val DX: int
    val DY: int
    new(dx, dy) = { DX = dx; DY = dy }

[<Struct>]
type EngineMessageArg =
    | Text of text: string
    | LocalizedKey of key: string
    | Int of intValue: int
    | Bool of boolValue: bool
    | Decimal of decimalValue: decimal

[<Struct>]
type EngineMessage =
    { Key: string
      Args: (string * EngineMessageArg)[] }
 
type ChangeSlot =
    | BaseTile = 0
    | Fixture = 1
    | Actor = 2
    | Item = 3
    | Decal = 4

[<Struct>]
type ChangedEntity =
    { Position: GridPos
      Slot: ChangeSlot
      EntityId: int option
      LocalObjectId: int option }

[<Struct>]
type EngineChangeSet =
    { ChangedBaseCells: GridPos[]
      ChangedLayerCells: GridPos[]
      ChangedEntities: ChangedEntity[]
      VisibilityInputChanged: bool
      OcclusionInputChanged: bool
      SaveRelevant: bool }
    static member Empty =
        { ChangedBaseCells = [||]
          ChangedLayerCells = [||]
          ChangedEntities = [||]
          VisibilityInputChanged = false
          OcclusionInputChanged = false
          SaveRelevant = false }

[<Struct>]
type InteractionResult =
    { Succeeded: bool
      Message: EngineMessage option
      TargetPosition: GridPos option
      TargetSlot: ChangeSlot option
      Changes: EngineChangeSet }
    static member Nothing =
        { Succeeded = false
          Message = None
          TargetPosition = None
          TargetSlot = None
          Changes = EngineChangeSet.Empty }
 
type MovementBlockedCause =
    | NoMovement = 0
    | MissingPlayerActor = 1
    | DestinationOutOfBounds = 2
    | DestinationNotWalkable = 3
    | DestinationOccupiedByActor = 4
    | DestinationBlockedByFixture = 5
    | MoveableRequiresStrength = 6
    | MoveablePushDestinationOutOfBounds = 7
    | MoveablePushDestinationBlocked = 8
    | SwapBlocked = 9
    | PlayerActorNotAtPosition = 10
 
type MovementKind =
    | Normal = 0
    | PushedMoveable = 1
    | SwappedMoveable = 2

[<Struct>]
type MovedMapObject =
    { Slot: ChangeSlot
      EntityId: int option
      LocalObjectId: int option
      OldPosition: GridPos
      NewPosition: GridPos }

[<Struct>]
type MovementResult =
    { Succeeded: bool
      Kind: MovementKind
      BlockedCause: MovementBlockedCause option
      Message: EngineMessage option
      PlayerOldPosition: GridPos
      PlayerNewPosition: GridPos
      MovedObject: MovedMapObject option
      Changes: EngineChangeSet }
    member this.ToBool() = this.Succeeded

// Free-sprite region on a globally-registered sprite sheet/atlas
[<Struct>]
type SpriteSheetCell =
    val SheetId: int
    val Row: int
    val Column: int
    new(sheetId, row, column) =
        { SheetId = sheetId
          Row = row
          Column = column }

[<Struct>]
type SpriteSheetRegion = { SheetId: int; X: int; Y: int; Width: int; Height: int }

// For rectangular blocks (efficient, contiguous)
[<Struct>]
type SpriteSheetSpan = 
    { TopLeft: SpriteSheetCell
      WidthCells: int
      HeightCells: int }

// Sprites can come from a global sheet/atlas cell, a standalone texture, or a scene/prefab.
type SpriteRef =
    | SheetRegion of SpriteSheetRegion // arbitrary pixel region on a sheet
    | SheetCell of SpriteSheetCell     // grid address on a registered sheet
    | SheetSpan of SpriteSheetSpan     // rectangular block of cells
    | SheetCells of SpriteSheetCell[]  // Ordered list for scrambled/irregular multi-cells
    | TextureId of int                 // maps to a loaded Texture2D managed externally
    | Scene of string                  // PackedScene path (e.g., "res://.../foo.tscn")

// Minimal NPC sheet support: 2 facings x 2 states.
[<Struct>]
type NpcFrames = { 
    NormalLeft: SpriteRef
    NormalRight: SpriteRef
    AttackLeft: SpriteRef
    AttackRight: SpriteRef 
} with static member Default = { 
        NormalLeft = SheetCell(SpriteSheetCell(0,0,0))
        NormalRight = SheetCell(SpriteSheetCell(0,0,0))
        AttackLeft = SheetCell(SpriteSheetCell(0,0,0))
        AttackRight = SheetCell(SpriteSheetCell(0,0,0)) }
            
type ActorFacing =
    | Left = 0
    | Right = 1

type ActorPose =
    | Normal = 0
    | Attack = 1 

type ActorState =
    | Idle = 0
    | Walking = 1
    | Attacking = 2
    | Spellcasting = 3
    | Normal = 4

[<Struct>]
type PlayerVisualState = {
    mutable Facing: ActorFacing
    mutable State: ActorPose
}

type Biome =
    | None = 0
    | Forest = 1
    | Desert = 2
    | Snow = 3
    | Swamp = 4
    | Mountain = 5
    | Ocean = 6
    | Plains = 7     

type TileType =
    | NullTile = 0
    | Void = 1
    | Wall = 2
    | Floor = 3
    | Ground = 4
    | Door = 5
    | Lever = 6
    | Stairs = 7
    | Water = 8
    | Lava = 9
    | CityOrTown = 10
    | Fixture = 11
    | Container = 12
    | Sign = 13 

type TileOpacity =
    | Opaque = 0uy
    | Transparent = 1uy
    | Air = 2uy
    | Translucent = 3uy

module TileOpacity = 
    let inline isOpaque(opacity: TileOpacity) =
        match opacity with
        | TileOpacity.Opaque -> true
        | _ -> false 
    let inline isTransparent(opacity: TileOpacity) = not (isOpaque opacity) 


type MapType =
    | Room = 0
    | ComplexBuilding = 1
    | TownOrCity = 2
    | Dungeon = 3
    | Overworld = 4

[<Struct>] type ClosedDoorState = {mutable Locked: bool}

type ComplexState =  
    | ClosedDoor of ClosedDoorState

    member this.IsDoorLocked() =
        match this with
        | ClosedDoor state -> state.Locked        

[<Struct>]
type TileVisualEntry = {
    Key: string
    SpriteLoc: SpriteLoc 
}

[<Struct>]
type TileProperties =
    { Walkable: bool 
      Interactable: bool
      TileType: TileType
      Health: int
      DescriptionKey: string
      Biome: Biome
      TileOpacity: TileOpacity
      Visuals: TileVisualEntry[]
      DestroyedSpriteLoc: SpriteLoc option 
      NextStateSpriteLoc: SpriteLoc option
      ComplexState: ComplexState option }
      
    static member NullTile =
        { Walkable = false 
          Interactable = false
          TileType = TileType.NullTile
          Health = -1
          DescriptionKey = ""
          Biome = Biome.None
          Visuals = [||]
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          TileOpacity = TileOpacity.Opaque
          ComplexState = None }
