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
            let mutable tile = Unchecked.defaultof<TileProperties>
            if properties.TryGetValue(spriteLoc, &tile) then tile else TileProperties.NullTile

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
    
    member _.ContainsKey(spriteLoc) = properties.ContainsKey spriteLoc

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

[<Struct>]
type internal ActorMoveTransaction =
    { Succeeded: bool
      Kind: MovementKind
      BlockedCause: MovementBlockedCause
      MovedFixtureId: int
      FixtureOldPosition: GridPos
      FixtureNewPosition: GridPos
      OpacityChanged: bool }


type TileMap =
    val mutable Width: int
    val mutable Height: int
    val Tiles: Tile[] // Changed from Tile[][]
    val EffectiveOpacity: TileOpacity[]
    val mutable EffectiveOpacityInitialized: bool
    val mutable VoidSpriteLoc: SpriteLoc
    val mutable MapName: string
    val mutable MapType: MapType
    val mutable TileSetName: string
    val mutable LayerCells: LayerCell[]
    val mutable SpawnPoints: (int * int)[]
    val Explored: byte[]

    new(width, height, tiles:Tile[], layercells:LayerCell[], voidSpriteLoc:SpriteLoc, tileSetName:string, mapname:string, mapType:MapType) =
        // Do NOT touch the tileset registry in the constructor.
        // Some map-loading paths construct TileMap before tilesets are registered.
        let effectiveOpacity = Array.create (width * height) TileOpacity.Opaque

        { Width = width
          Height = height
          Tiles = tiles
          EffectiveOpacity = effectiveOpacity
          EffectiveOpacityInitialized = false
          MapName = mapname
          MapType = mapType
          TileSetName = tileSetName
          VoidSpriteLoc = voidSpriteLoc
          LayerCells = layercells
          SpawnPoints = Array.create 10 (-1, -1)
          Explored = Array.zeroCreate (width * height) }

    new(width, height, voidSpriteLoc, tileSetName, mapname, mapType) =
        let tiles = Array.init (width * height) (fun _ -> { SpriteLoc = SpriteLoc(0, 0, 0); Health = 0; IsOccupied = false })
        let layercells = Array.init (width * height) (fun _ -> LayerCell.Create())
        // See note in other ctor: avoid tileset registry access here.
        let effectiveOpacity = Array.create (width * height) TileOpacity.Opaque

        { Width = width
          Height = height
          Tiles = tiles
          EffectiveOpacity = effectiveOpacity
          EffectiveOpacityInitialized = false
          MapName = mapname
          MapType = mapType
          TileSetName = tileSetName
          VoidSpriteLoc = voidSpriteLoc
          LayerCells = layercells
          SpawnPoints = Array.create 10 (-1, -1)
          Explored = Array.zeroCreate (width * height) }

    member inline private this.GetIndex(x: int, y: int) = y * this.Width + x
    member inline this.GetLayerCell(x: int, y: int) = this.LayerCells.[this.GetIndex(x, y)]

    member this.InitEffectiveOpacityCache() =
        // Call once tileset registry is ready (after map load / tileset registration).
        // Safe to call multiple times.
        let tileset = TilesetRegistry.get this.TileSetName
        for idx in 0 .. this.EffectiveOpacity.Length - 1 do
            let baseOpacity = tileset.[this.Tiles.[idx].SpriteLoc].TileOpacity
            this.EffectiveOpacity.[idx] <- LayerQueries.EffectiveTileOpacity(baseOpacity, this.LayerCells.[idx])
        this.EffectiveOpacityInitialized <- true

    member inline private this.RecomputeEffectiveOpacityAtIndex(idx: int) =
        let tile = this.Tiles.[idx]
        let baseOpacity = TilesetRegistry.get(this.TileSetName).[tile.SpriteLoc].TileOpacity
        let cell = this.LayerCells.[idx]
        this.EffectiveOpacity.[idx] <- LayerQueries.EffectiveTileOpacity(baseOpacity, cell)

    member inline private this.RecomputeEffectiveOpacityAt(x: int, y: int) =
        this.RecomputeEffectiveOpacityAtIndex(this.GetIndex(x, y))

    member inline this.GetOpacityByIndex(index: int) : TileOpacity =
        this.EffectiveOpacity.[index]

    member this.MarkExplored(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            this.Explored.[this.GetIndex(x, y)] <- 1uy

    member this.IsExplored(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then false
        else this.Explored.[this.GetIndex(x, y)] <> 0uy

    member this.ClearExplored() =
        System.Array.Clear(this.Explored, 0, this.Explored.Length)

    member this.Update(x, y, tile) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            this.Tiles.[this.GetIndex(x, y)] <- tile
            this.RecomputeEffectiveOpacityAt(x, y)

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

    member this.TryGetTileDescriptionKey(x: int, y: int) : string option =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then None
        else
            let props = this.GetTileProperties(x, y)
            if System.String.IsNullOrEmpty(props.DescriptionKey) then None
            else Some props.DescriptionKey

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
            this.RecomputeEffectiveOpacityAt(x, y)

    member this.ClearFixture(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.FixtureId <- None
            this.RecomputeEffectiveOpacityAt(x, y)

    member this.SetActor(x: int, y: int, actorId: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.ActorId <- Some actorId
            this.RecomputeEffectiveOpacityAt(x, y)

    member this.ClearActor(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            cell.ActorId <- None
            this.RecomputeEffectiveOpacityAt(x, y)

    member inline private _.BlockedActorMove(cause: MovementBlockedCause) : ActorMoveTransaction =
        { Succeeded = false
          Kind = MovementKind.Normal
          BlockedCause = cause
          MovedFixtureId = -1
          FixtureOldPosition = GridPos(0, 0)
          FixtureNewPosition = GridPos(0, 0)
          OpacityChanged = false }

    member inline private _.SucceededActorMove(kind, movedFixtureId, fixtureOldPosition, fixtureNewPosition, opacityChanged) : ActorMoveTransaction =
        { Succeeded = true
          Kind = kind
          BlockedCause = MovementBlockedCause.NoMovement
          MovedFixtureId = movedFixtureId
          FixtureOldPosition = fixtureOldPosition
          FixtureNewPosition = fixtureNewPosition
          OpacityChanged = opacityChanged }

    member inline private _.GetFixtureMovementInfo(fixtureId: int) =
        let mutable spriteProps = Unchecked.defaultof<SpriteProperties>
        if EntityRegistry.SpriteProps.TryGetValue(fixtureId, &spriteProps) then
            match spriteProps.SpriteType with
            | SpriteType.Fixture fixtureProps -> struct (fixtureProps.BlocksMovement, fixtureProps.Moveable)
            | _ -> struct (false, 0)
        else
            struct (true, 0)

    member inline private this.CommitNormalActorMove(actorIndex: int, destIndex: int, actorId: int) : ActorMoveTransaction =
        let actorCell = this.LayerCells.[actorIndex]
        let destCell = this.LayerCells.[destIndex]
        let oldActorOpacity = this.EffectiveOpacity.[actorIndex]
        let oldDestOpacity = this.EffectiveOpacity.[destIndex]
        actorCell.ActorId <- None
        destCell.ActorId <- Some actorId
        this.RecomputeEffectiveOpacityAtIndex(actorIndex)
        this.RecomputeEffectiveOpacityAtIndex(destIndex)
        let opacityChanged =
            oldActorOpacity <> this.EffectiveOpacity.[actorIndex]
            || oldDestOpacity <> this.EffectiveOpacity.[destIndex]
        this.SucceededActorMove(MovementKind.Normal, -1, GridPos(0, 0), GridPos(0, 0), opacityChanged)

    member inline private this.CommitActorFixtureSwap(actorIndex: int, destIndex: int, actorId: int, actorPosition: GridPos, fixtureId: int, fixturePosition: GridPos) : ActorMoveTransaction =
        let actorCell = this.LayerCells.[actorIndex]
        if actorCell.FixtureId.IsSome then
            this.BlockedActorMove(MovementBlockedCause.SwapBlocked)
        else
            let destCell = this.LayerCells.[destIndex]
            let oldActorOpacity = this.EffectiveOpacity.[actorIndex]
            let oldFixtureOpacity = this.EffectiveOpacity.[destIndex]
            actorCell.ActorId <- None
            actorCell.FixtureId <- Some fixtureId
            destCell.FixtureId <- None
            destCell.ActorId <- Some actorId
            this.RecomputeEffectiveOpacityAtIndex(actorIndex)
            this.RecomputeEffectiveOpacityAtIndex(destIndex)
            let opacityChanged =
                oldActorOpacity <> this.EffectiveOpacity.[actorIndex]
                || oldFixtureOpacity <> this.EffectiveOpacity.[destIndex]
            this.SucceededActorMove(MovementKind.SwappedMoveable, fixtureId, fixturePosition, actorPosition, opacityChanged)

    member internal this.TryMoveActorTransaction(actorId: int, actorX: int, actorY: int, destX: int, destY: int, deltaX: int, deltaY: int, actorStrength: int) : ActorMoveTransaction =
        if destX < 0 || destX >= this.Width || destY < 0 || destY >= this.Height then
            this.BlockedActorMove(MovementBlockedCause.DestinationOutOfBounds)
        elif actorX < 0 || actorX >= this.Width || actorY < 0 || actorY >= this.Height then
            this.BlockedActorMove(MovementBlockedCause.PlayerActorNotAtPosition)
        else
            let actorIndex = this.GetIndex(actorX, actorY)
            let destIndex = this.GetIndex(destX, destY)
            let actorCell = this.LayerCells.[actorIndex]
            let destCell = this.LayerCells.[destIndex]

            match actorCell.ActorId with
            | Some sourceActorId when sourceActorId = actorId ->
                if not (this.GetTileProperties(destX, destY).Walkable) then
                    this.BlockedActorMove(MovementBlockedCause.DestinationNotWalkable)
                elif destCell.ActorId.IsSome then
                    this.BlockedActorMove(MovementBlockedCause.DestinationOccupiedByActor)
                else
                    match destCell.FixtureId with
                    | Some fixtureId ->
                        let struct (blocksMovement, moveRequirement) = this.GetFixtureMovementInfo(fixtureId)
                        if not blocksMovement then
                            if this.Tiles.[destIndex].IsOccupied then
                                this.BlockedActorMove(MovementBlockedCause.PlayerActorNotAtPosition)
                            else
                                this.CommitNormalActorMove(actorIndex, destIndex, actorId)
                        elif moveRequirement <= 0 then
                            this.BlockedActorMove(MovementBlockedCause.DestinationBlockedByFixture)
                        elif moveRequirement > actorStrength then
                            this.BlockedActorMove(MovementBlockedCause.MoveableRequiresStrength)
                        else
                            let fixturePosition = GridPos(destX, destY)
                            let pushX = destX + deltaX
                            let pushY = destY + deltaY
                            let canPush =
                                pushX >= 0 && pushX < this.Width && pushY >= 0 && pushY < this.Height
                                && actorCell.FixtureId.IsNone
                                && this.GetTileProperties(pushX, pushY).Walkable

                            if canPush then
                                let pushIndex = this.GetIndex(pushX, pushY)
                                let pushCell = this.LayerCells.[pushIndex]
                                if not this.Tiles.[pushIndex].IsOccupied
                                   && pushCell.ActorId.IsNone
                                   && pushCell.FixtureId.IsNone then
                                    let oldActorOpacity = this.EffectiveOpacity.[actorIndex]
                                    let oldFixtureOpacity = this.EffectiveOpacity.[destIndex]
                                    let oldPushOpacity = this.EffectiveOpacity.[pushIndex]
                                    actorCell.ActorId <- None
                                    destCell.FixtureId <- None
                                    destCell.ActorId <- Some actorId
                                    pushCell.FixtureId <- Some fixtureId
                                    this.RecomputeEffectiveOpacityAtIndex(actorIndex)
                                    this.RecomputeEffectiveOpacityAtIndex(destIndex)
                                    this.RecomputeEffectiveOpacityAtIndex(pushIndex)
                                    let opacityChanged =
                                        oldActorOpacity <> this.EffectiveOpacity.[actorIndex]
                                        || oldFixtureOpacity <> this.EffectiveOpacity.[destIndex]
                                        || oldPushOpacity <> this.EffectiveOpacity.[pushIndex]
                                    this.SucceededActorMove(MovementKind.PushedMoveable, fixtureId, fixturePosition, GridPos(pushX, pushY), opacityChanged)
                                else
                                    this.CommitActorFixtureSwap(actorIndex, destIndex, actorId, GridPos(actorX, actorY), fixtureId, fixturePosition)
                            else
                                this.CommitActorFixtureSwap(actorIndex, destIndex, actorId, GridPos(actorX, actorY), fixtureId, fixturePosition)
                    | None ->
                        if this.Tiles.[destIndex].IsOccupied then
                            this.BlockedActorMove(MovementBlockedCause.PlayerActorNotAtPosition)
                        else
                            this.CommitNormalActorMove(actorIndex, destIndex, actorId)
            | _ ->
                this.BlockedActorMove(MovementBlockedCause.PlayerActorNotAtPosition)

    member this.TryMoveActor(x: int, y: int, x2: int, y2: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height ||
           x2 < 0 || x2 >= this.Width || y2 < 0 || y2 >= this.Height then
            false
        else
            let src = this.GetLayerCell(x, y)
            match src.ActorId with
            | None -> false
            | Some aid ->
                if not (this.IsWalkable(x2, y2)) then false
                elif this.IsOccupied(x2, y2) then false
                else
                    let dst = this.GetLayerCell(x2, y2)
                    src.ActorId <- None
                    dst.ActorId <- Some aid
                    this.RecomputeEffectiveOpacityAt(x, y)
                    this.RecomputeEffectiveOpacityAt(x2, y2)
                    true

    member this.MoveActor(x: int, y: int, x2: int, y2: int) =
        this.TryMoveActor(x, y, x2, y2) |> ignore

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
        this.TryMoveFixture(x, y, x2, y2) |> ignore
    
    /// Move the fixture at (x,y) to (x2,y2). Clears fixture at (x,y), sets at (x2,y2).
    /// Returns true if a fixture was moved, false if none was present or destination is blocked.
    member this.TryMoveFixture(x: int, y: int, x2: int, y2: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height ||
           x2 < 0 || x2 >= this.Width || y2 < 0 || y2 >= this.Height then
            false
        else
            let src = this.GetLayerCell(x, y)
            match src.FixtureId with
            | None -> false
            | Some fid ->
                let blocks = SpritePropsQueries.checkFixtureBlocksMovement(EntityRegistry.SpriteProps[fid].SpriteType)
                // If it blocks, validate that the destination is walkable and not already occupied.
                if blocks && (not (this.IsWalkable(x2, y2)) || this.IsOccupied(x2, y2)) then
                    false
                else
                    let dst = this.GetLayerCell(x2, y2)
                    src.FixtureId <- None
                    dst.FixtureId <- Some fid
                    // Trigger recompute once per tile now that state is updated
                    this.RecomputeEffectiveOpacityAt(x, y)
                    this.RecomputeEffectiveOpacityAt(x2, y2)
                    true

    /// Atomically pushes the fixture at (fixtureX,fixtureY) to (fixtureDestX,fixtureDestY)
    /// and moves the actor at (actorX,actorY) into the fixture's old cell.
    member this.TryPushFixtureAndMoveActor(actorX: int, actorY: int, fixtureX: int, fixtureY: int, fixtureDestX: int, fixtureDestY: int) : bool =
        if actorX < 0 || actorX >= this.Width || actorY < 0 || actorY >= this.Height ||
           fixtureX < 0 || fixtureX >= this.Width || fixtureY < 0 || fixtureY >= this.Height ||
           fixtureDestX < 0 || fixtureDestX >= this.Width || fixtureDestY < 0 || fixtureDestY >= this.Height then
            false
        elif not (this.IsWalkable(fixtureX, fixtureY)) || not (this.IsWalkable(fixtureDestX, fixtureDestY)) then
            false
        elif this.IsOccupied(fixtureDestX, fixtureDestY) then
            false
        else
            let actorCell = this.GetLayerCell(actorX, actorY)
            let fixtureCell = this.GetLayerCell(fixtureX, fixtureY)
            let fixtureDestCell = this.GetLayerCell(fixtureDestX, fixtureDestY)
            match actorCell.ActorId, actorCell.FixtureId, fixtureCell.ActorId, fixtureCell.FixtureId, fixtureDestCell.ActorId, fixtureDestCell.FixtureId with
            | Some actorId, None, None, Some fixtureId, None, None ->
                actorCell.ActorId <- None
                fixtureCell.FixtureId <- None
                fixtureCell.ActorId <- Some actorId
                fixtureDestCell.FixtureId <- Some fixtureId
                this.RecomputeEffectiveOpacityAt(actorX, actorY)
                this.RecomputeEffectiveOpacityAt(fixtureX, fixtureY)
                this.RecomputeEffectiveOpacityAt(fixtureDestX, fixtureDestY)
                true
            | _ -> false

    /// Swaps the actor at (actorX,actorY) with the fixture at (fixtureX,fixtureY).
    /// This is intended for movement rules such as moveable push/swap where the
    /// occupied actor cell is the fixture destination.
    member this.TrySwapActorAndFixture(actorX: int, actorY: int, fixtureX: int, fixtureY: int) : bool =
        if actorX < 0 || actorX >= this.Width || actorY < 0 || actorY >= this.Height ||
           fixtureX < 0 || fixtureX >= this.Width || fixtureY < 0 || fixtureY >= this.Height then
            false
        else
            let actorCell = this.GetLayerCell(actorX, actorY)
            let fixtureCell = this.GetLayerCell(fixtureX, fixtureY)
            match actorCell.ActorId, actorCell.FixtureId, fixtureCell.ActorId, fixtureCell.FixtureId with
            | Some actorId, None, None, Some fixtureId ->
                if not (this.IsWalkable(fixtureX, fixtureY)) then
                    false
                else
                    actorCell.ActorId <- None
                    actorCell.FixtureId <- Some fixtureId
                    fixtureCell.FixtureId <- None
                    fixtureCell.ActorId <- Some actorId
                    this.RecomputeEffectiveOpacityAt(actorX, actorY)
                    this.RecomputeEffectiveOpacityAt(fixtureX, fixtureY)
                    true
            | _ -> false

    member this.AddDecal(x: int, y: int, decalId: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            LayerQueries.AddDecal(cell, decalId)

    member this.ClearDecals(x: int, y: int) =
        if x >= 0 && x < this.Width && y >= 0 && y < this.Height then
            let cell = this.GetLayerCell(x, y)
            LayerQueries.ClearDecals cell

    member this.GetDecal(x: int, y: int) =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then None
        else LayerQueries.TryGetTopDecalId(this.GetLayerCell(x, y))

    member this.GetDecals(x: int, y: int) : LayerQueries.DecalView =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then
            { Decals = Array.empty
              Count = 0 }
        else
            LayerQueries.GetDecalView(this.GetLayerCell(x, y))

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
                    match cell.FixtureId with
                    | None -> false
                    | Some fid -> SpritePropsQueries.checkFixtureBlocksMovement(EntityRegistry.SpriteProps[fid].SpriteType)

    member this.GetOpacity(x: int, y: int) : TileOpacity =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then TileOpacity.Opaque
        else
            this.EffectiveOpacity.[this.GetIndex(x, y)]

    member this.IsOpaque(x: int, y: int) : bool =
        if x < 0 || x >= this.Width || y < 0 || y >= this.Height then true
        else
            match this.EffectiveOpacity.[this.GetIndex(x, y)] with
            | TileOpacity.Opaque -> true
            | _ -> false

[<Struct>]
type TileUpdate = { X: int; Y: int; Tile: Tile }
