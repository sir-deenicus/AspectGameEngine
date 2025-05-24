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
      TileOpacity: TileOpacity }
    static member NullTile = { Walkable = false; IsVoid = false; TileType = TileType.NullTile; Health = 0; Description = ""; TileOpacity = TileOpacity.Opaque }
