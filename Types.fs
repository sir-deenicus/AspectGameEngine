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
    | NotOutside = 0
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
    | Door = 4
    | Stairs = 5
    | Water = 6
    | Lava = 7
    | City = 8

type TileOpacity =
    | Opaque = 0
    | Transparent = 1

type MapType =
    | Dungeon = 0
    | Overworld = 1
    | ComplexBuilding = 2
    | Room = 3
    | CityOrTown = 4

[<Struct>]
type TileProperties =
    { Walkable: bool
      IsVoid: bool
      TileType: TileType
      Health: int
      Description: string
      Biome: Biome
      TileOpacity: TileOpacity }

    static member NullTile =
        { Walkable = false
          IsVoid = false
          TileType = TileType.NullTile
          Health = 0
          Description = ""
          Biome = Biome.NotOutside
          TileOpacity = TileOpacity.Opaque }
