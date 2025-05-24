namespace AspectGameEngine

open System.Collections.Generic

type TilePropertiesReference() =
    let properties = new Dictionary<SpriteLoc, TileProperties>()

    member _.Item
        with get spriteLoc =
            let res, tile = properties.TryGetValue spriteLoc
            if res then tile else TileProperties.NullTile

        and set spriteLoc value =
            if properties.ContainsKey spriteLoc then
                properties.[spriteLoc] <- value
            else
                properties.Add(spriteLoc, value)

    member _.Update(spriteLoc, tileProperties) =
        if properties.ContainsKey spriteLoc then
            properties.[spriteLoc] <- tileProperties
        else
            properties.Add(spriteLoc, tileProperties)

[<Struct>]
type Tile =
    { SpriteLoc: SpriteLoc
      Health: int
      DestroyedSpriteLoc: SpriteLoc option }

type TileMap =
    val mutable Width: int
    val mutable Height: int
    val Tiles: Tile[][]
    val mutable VoidSpriteLoc: SpriteLoc
    val mutable MapName: string
    val mutable MapType: MapType
    val mutable TilePropertiesReference: TilePropertiesReference

    new(width, height, tiles, voidSpriteLoc, tilePropertiesReference) =
        { Width = width
          Height = height
          Tiles = tiles
          MapName = ""
          MapType = MapType.Room
          TilePropertiesReference = tilePropertiesReference
          VoidSpriteLoc = voidSpriteLoc }

    member this.Update(x, y, tile) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            this.Tiles.[y].[x] <- tile

    member this.GetTileProperties(x: int, y: int) : TileProperties =
        let tile = this.Tiles.[y].[x]
        this.TilePropertiesReference.[tile.SpriteLoc]

    member this.IsWalkable(x: int, y: int) : bool = (this.GetTileProperties(x, y)).Walkable

    member this.IsVoid(x: int, y: int) : bool = (this.GetTileProperties(x, y)).IsVoid

    member this.IsOpaque(x: int, y: int) : bool =
        (this.GetTileProperties(x, y)).TileOpacity = TileOpacity.Opaque
