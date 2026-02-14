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

type TileOpacity =
    | Opaque = 0
    | Transparent = 1
    | Air = 2
    | Translucent = 3

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
type TileProperties =
    { Walkable: bool 
      Interactable: bool
      TileType: TileType
      Health: int
      DescriptionKey: string
      Biome: Biome
      TileOpacity: TileOpacity
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
          DestroyedSpriteLoc = None
          NextStateSpriteLoc = None
          TileOpacity = TileOpacity.Opaque
          ComplexState = None }
