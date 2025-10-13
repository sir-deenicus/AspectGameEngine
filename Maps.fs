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

    // Expose properties for serialization
    member _.GetAllProperties() = properties :> seq<KeyValuePair<SpriteLoc, TileProperties>>


[<Struct>]
type Tile =
    { SpriteLoc: SpriteLoc
      mutable Health: int
      IsOccupied: bool }


type TileMap =
    val mutable Width: int
    val mutable Height: int
    val Tiles: Tile[] // Changed from Tile[][]
    val mutable VoidSpriteLoc: SpriteLoc
    val mutable MapName: string
    val mutable MapType: MapType
    val mutable TilePropertiesReference: TilePropertiesReference

    new(width, height, tiles, voidSpriteLoc, tilePropertiesReference, mapname, mapType) =
        { Width = width
          Height = height
          Tiles = tiles
          MapName = mapname
          MapType = mapType
          TilePropertiesReference = tilePropertiesReference
          VoidSpriteLoc = voidSpriteLoc }

    member inline private this.GetIndex(x: int, y: int) = y * this.Width + x

    member this.Update(x, y, tile) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            this.Tiles.[this.GetIndex(x, y)] <- tile

    member this.GetTile(x: int, y: int) : Tile =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            { SpriteLoc = this.VoidSpriteLoc
              Health = 0
              IsOccupied = false }
        else
            this.Tiles.[this.GetIndex(x, y)]

    member this.GetTileProperties(x: int, y: int) : TileProperties =
        let tile = this.Tiles.[this.GetIndex(x, y)]
        this.TilePropertiesReference.[tile.SpriteLoc]

    member this.IsWalkable(x: int, y: int) : bool = (this.GetTileProperties(x, y)).Walkable

    member this.IsOccupied(x: int, y: int) : bool = this.GetTile(x, y).IsOccupied

    member this.IsOpaque(x: int, y: int) : bool =
        (this.GetTileProperties(x, y)).TileOpacity = TileOpacity.Opaque


[<Struct>]
type TileUpdate = { X: int; Y: int; Tile: Tile }
