namespace AspectGameEngine

open System.Collections.Generic

type TilePropertiesReference(?tileSetName) =
    let properties = new Dictionary<SpriteLoc, TileProperties>()
    let mutable tilesetname = defaultArg tileSetName "" 

    member _.TileSetName
        with get() = tilesetname
        and set(value) = tilesetname <- value
    
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
    

// Minimal registry to share tileset data 
module TilesetRegistry = 
    let private registry = Dictionary<string, TilePropertiesReference>()
 
    let register (name:string) (ref:TilePropertiesReference) = registry[name] <- ref 

    // Try get, and Get (throws if missing)
    let tryGet (name:string) =
        match registry.TryGetValue(name) with
        | true, v -> Some v
        | _ -> None 

    let get (name:string) = registry[name]


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
    val mutable TileSetName: string
    val mutable LayerCells: LayerCell[]

    new(width, height, tiles, layercells, voidSpriteLoc, tileSetName, mapname, mapType) =
        { Width = width
          Height = height
          Tiles = tiles
          MapName = mapname
          MapType = mapType
          TileSetName = tileSetName
          VoidSpriteLoc = voidSpriteLoc
          LayerCells = layercells }

    new(width, height, voidSpriteLoc, tileSetName, mapname, mapType) =
        let tiles = Array.init (width * height) (fun _ -> { SpriteLoc = SpriteLoc(0, 0, 0); Health = 0; IsOccupied = false })
        let layercells = Array.init (width * height) (fun _ -> LayerCell.Create())
        { Width = width
          Height = height
          Tiles = tiles
          MapName = mapname
          MapType = mapType
          TileSetName = tileSetName
          VoidSpriteLoc = voidSpriteLoc
          LayerCells = layercells }

    member inline private this.GetIndex(x: int, y: int) = y * this.Width + x
    member inline this.GetLayerCell(x: int, y: int) = this.LayerCells.[this.GetIndex(x, y)]

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
        TilesetRegistry.get(this.TileSetName)[tile.SpriteLoc]

    member this.AddItem(x: int, y: int, itemId: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then false
        else
            let cell = this.GetLayerCell(x, y)
            if cell.Items.Count < EntityRegistry.MaxItemsPerTile then
                cell.Items.Add itemId
                true
            else
                false

    member this.GetRenderItems(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            { Items = ResizeArray(); Start = 0; Count = 0 }
        else
            LayerQueries.GetRenderItemView(this.GetLayerCell(x, y))

    member this.SetFixture(x: int, y: int, fixtureId: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.FixtureId <- Some fixtureId

    member this.ClearFixture(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.FixtureId <- None

    member this.SetActor(x: int, y: int, actorId: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.ActorId <- Some actorId

    member this.ClearActor(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.ActorId <- None

    member this.TryGetActor(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then None
        else (this.GetLayerCell(x, y)).ActorId

    member this.TryGetFixture(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then None
        else (this.GetLayerCell(x, y)).FixtureId
 
    member this.GetActor(x: int, y: int) =
        this.GetLayerCell(x, y).ActorId

    member this.GetFixture(x: int, y: int) =
        this.GetLayerCell(x, y).FixtureId 

    member this.MoveFixture(x: int, y: int, x2: int, y2: int) =
        let fid = this.GetLayerCell(x, y).FixtureId
        if not EntityRegistry.FixtureProps[fid.Value].BlocksMovement then 
            this.GetLayerCell(x, y).FixtureId <- None
            this.GetLayerCell(x2, y2).FixtureId <- fid 

    member this.SetDecal(x: int, y: int, decalId: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.DecalId <- Some decalId

    member this.ClearDecal(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.DecalId <- None

    member this.GetDecal(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then None
        else (this.GetLayerCell(x, y)).DecalId

    member this.IsWalkable(x: int, y: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then false
        else (this.GetTileProperties(x, y)).Walkable             

    member this.IsOccupied(x: int, y: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then true
        else
            let tile = this.GetTile(x, y)
            let cell = this.GetLayerCell(x, y)
            // Consider base tile occupancy first
            if tile.IsOccupied then true
            else
                // Actor present => occupied
                match cell.ActorId with
                | Some _ -> true
                | None ->
                    // Fixture only counts as occupying if it blocks movement
                    match cell.FixtureId with
                    | Some fid ->
                        match EntityRegistry.FixtureProps.TryGetValue fid with
                        | true, fp when fp.BlocksMovement -> true
                        | _ -> false
                    | None -> false

    member this.IsOpaque(x: int, y: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then true
        else
            let baseOpacity = (this.GetTileProperties(x, y)).TileOpacity
            let cell = this.GetLayerCell(x, y)
            match LayerQueries.EffectiveTileOpacity(baseOpacity, cell) with
            | TileOpacity.Opaque -> true
            | _ -> false

        /// Move the fixture at (x,y) to (x2,y2). Clears fixture at (x,y), sets at (x2,y2).
        /// Returns true if a fixture was moved, false if none was present.
        member this.TryMoveFixture(x: int, y: int, x2: int, y2: int) : bool =
            if x < 0 || x >= this.Width || y < 0 || y >= this.Height ||
               x2 < 0 || x2 >= this.Width || y2 < 0 || y2 >= this.Height then
                false
            else
                match this.TryGetFixture(x, y) with
                | Some fid ->
                    this.ClearFixture(x, y)
                    this.SetFixture(x2, y2, fid)
                    true
                | None -> false 

[<Struct>]
type TileUpdate = { X: int; Y: int; Tile: Tile }
