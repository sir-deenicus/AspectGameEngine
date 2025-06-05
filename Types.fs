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

type Biome =
    | None = 0
    | Forest = 1
    | Desert = 2
    | Snow = 3
    | Swamp = 4
    | Mountain = 5
    | Ocean = 6
    | Plains = 7 
    | AlchemyLab = 8
    | EnchantersLab = 9
    | Blacksmith = 10

type TileType =
    | NullTile = 0
    | Void = 1
    | Wall = 2
    | Floor = 3
    | Door = 4
    | Lever = 5
    | Stairs = 6
    | Water = 7
    | Lava = 8
    | City = 9

type TileOpacity =
    | Opaque = 0
    | Transparent = 1

type MapType =
    | Room = 0
    | ComplexBuilding = 1
    | TownOrCity = 2
    | Dungeon = 3
    | Overworld = 4

[<Struct>]
type TileProperties =
    { Walkable: bool
      IsVoid: bool
      TileType: TileType
      Health: int
      DescriptionKey: string
      Biome: Biome
      TileOpacity: TileOpacity
      DestroyedSpriteLoc: SpriteLoc option 
      StateChangedSpriteLoc: SpriteLoc option }

    static member NullTile =
        { Walkable = false
          IsVoid = false
          TileType = TileType.NullTile
          Health = 0
          DescriptionKey = ""
          Biome = Biome.None
          DestroyedSpriteLoc = None
          StateChangedSpriteLoc = None
          TileOpacity = TileOpacity.Opaque }
