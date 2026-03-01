namespace AspectGameEngine

open System.Collections.Generic

module InteractionStrings = 
    [<Literal>]
    let DoorLocked = "door-locked"

    [<Literal>]
    let DoorUnlocked = "door-unlocked" 

[<Struct>]
type InteractResult =
    | Nothing
    | Msg of msgkey:string 

type LookAtResult =
    | Null = 0
    | SingleObject = 1
    | MultipleObjectsFullyListed = 2
    | MultipleObjectsIncompleteList = 3

[<Struct>]
type LookAtInfo = { 
    Result: LookAtResult
    LayerCount: byte
    HasMore: bool
    LayerKey0: string
    LayerKey1: string
    LayerKey2: string
    BaseKey: string 
}

[<Struct>]
type DoorAction =
    | LockDoor of keyid:int
    | UnLockDoor of keyid:int
    | OpenOrCloseDoor 

[<Struct>]
type InteractionType =
    | NullAction
    | DoorInteraction of action: DoorAction
 
type PlayerModel = { 
    mutable PlayerPos: GridPos
    PlayerActorId: int option
    mutable PlayerVisual: PlayerVisualState
    mutable PlayerFrames: NpcFrames
    mutable PlayerState: ActorState
}

type GameModel = {   
    Map: TileMap
    PlayerModel: PlayerModel
    VisibilityState: VisibilityState
    mutable VisibilityHalfWidth: int
    mutable VisibilityHalfHeight: int
    mutable VisibilityTranslucencyBudget: int

    // Per-tile *instance* state keyed by tileIndex (plain int).
    TileComplexStateInstance: Dictionary<int, ComplexState> 
} 

module Utils =
    let inline inBounds (map: TileMap) (pos: GridPos) =
        pos.X >= 0 && pos.X < map.Width && pos.Y >= 0 && pos.Y < map.Height

    let inline indexOf (map: TileMap) (pos: GridPos) = pos.Y * map.Width + pos.X
    let tryFindVisual (key: string) (visuals: TileVisualEntry[]) : SpriteLoc option =
        visuals
        |> Array.tryFind (fun v -> v.Key = key)
        |> Option.map (fun v -> v.SpriteLoc)

    let inline normalizeKey (key: string) =
        if System.String.IsNullOrEmpty(key) then "" else key

    let tryGetEntityDescKey (id: int) : string =
        match SpritePropsQueries.tryGet id with
        | None -> ""
        | Some sp ->
            match sp.SpriteType with
            | SpriteType.Actor ap -> normalizeKey ap.DescKey
            | SpriteType.Fixture fp -> normalizeKey fp.DescKey
            | SpriteType.Item ip -> normalizeKey ip.DescKey
            | SpriteType.Decal dp -> normalizeKey dp.DescKey

    let tryGetRenderLayer (id: int) : int =
        match SpritePropsQueries.tryGet id with
        | None -> 0
        | Some sp -> sp.RenderLayer
   
    
module Doors = 
    
    [<Literal>]
    let private DoorClosedKey = "door-closed"

    [<Literal>]
    let private DoorOpenedKey = "door-opened" 
    
    let  tryGetTileLocked (model: GameModel) (tileIndex: int) : bool =
        match model.TileComplexStateInstance.TryGetValue(tileIndex) with
        | true, s -> s.IsDoorLocked()
        | _ -> false

    let setTileLocked (model: GameModel) (tileIndex: int) (locked: bool) : unit =
        if locked then
            model.TileComplexStateInstance.[tileIndex] <- ComplexState.ClosedDoor { Locked = true }
        else
            model.TileComplexStateInstance.Remove(tileIndex) |> ignore 
    
    let tryInteractDoor (model: GameModel) (pos: GridPos) (interaction: DoorAction) : InteractResult =
        let map = model.Map
        if not (Utils.inBounds map pos) then Nothing
        else
            let tileIndex = Utils.indexOf map pos
            let tile = map.GetTile(pos.X, pos.Y)
            let props = map.GetTileProperties(pos.X, pos.Y)
            if props.TileType <> TileType.Door then Nothing
            else
                let tileSet = TilesetRegistry.get map.TileSetName
                match interaction with  
                | DoorAction.OpenOrCloseDoor ->
                    //we know that doors only have one state they point to. Get the zero index of current object's visual state:
                    let tileInfo = tileSet[tile.SpriteLoc]
                    if tileInfo.Visuals.Length = 0 then Nothing
                    else
                        let visualsInfo = tileInfo.Visuals[0]
                        let targetSpriteLoc = visualsInfo.SpriteLoc
                        //set spriteLoc at current tile
                        map.Update(pos.X, pos.Y, { tile with SpriteLoc = targetSpriteLoc })
                        InteractResult.Msg visualsInfo.Key // the key will be the name of the state we want to switch to
                | _ -> Nothing

module GameUpdate = 
    let recomputeVisibility (model: GameModel) : unit =
        RectFov.compute
            model.Map
            model.VisibilityState
            model.PlayerModel.PlayerPos
            model.VisibilityHalfWidth
            model.VisibilityHalfHeight
            model.VisibilityTranslucencyBudget

    let private resolvePlayerSprite (frames: NpcFrames) (visual: PlayerVisualState) : SpriteRef =
        match visual.State, visual.Facing with
        | ActorPose.Attack, ActorFacing.Right -> frames.AttackRight
        | ActorPose.Attack, _ -> frames.AttackLeft
        | _, ActorFacing.Right -> frames.NormalRight
        | _ -> frames.NormalLeft

    let private ensurePlayerActorRegistered (playerActorId: int) (frames: NpcFrames) (visual: PlayerVisualState) : unit =
        let spriteRef = resolvePlayerSprite frames visual
        let actorProps =
            { TileOpacity = TileOpacity.Transparent
              DescKey = "player"
              NpcFrames = Some frames }
        EntityRegistry.SpriteProps.[playerActorId] <-
            { Sprite = spriteRef
              SpriteType = SpriteType.Actor actorProps
              RenderLayer = 100 }

    let private createWith spawnPointIndex (initialMap: TileMap) (playerFrames: NpcFrames) (playerActorId: int option) : GameModel =
        let (x,y) = initialMap.SpawnPoints.[spawnPointIndex] 

        let playerVisual = { Facing = ActorFacing.Left; State = ActorPose.Normal }

        match playerActorId with
        | Some aid ->
            ensurePlayerActorRegistered aid playerFrames playerVisual
            initialMap.SetActor(x, y, aid)
        | None -> ()

        let playerModel = { 
            PlayerPos = GridPos(x,y)
            PlayerActorId = playerActorId
            PlayerVisual = playerVisual
            PlayerFrames = playerFrames
            PlayerState = ActorState.Idle
        }

        let visibilityHalfWidth = max 0 ((initialMap.Width + 1) / 2)
        let visibilityHalfHeight = max 0 ((initialMap.Height + 1) / 2)

        let model = {
            Map = initialMap
            PlayerModel = playerModel
            VisibilityState = VisibilityState(initialMap.Width, initialMap.Height)
            VisibilityHalfWidth = visibilityHalfWidth
            VisibilityHalfHeight = visibilityHalfHeight
            VisibilityTranslucencyBudget = 0
            TileComplexStateInstance = Dictionary<int, ComplexState>() 
        }

        recomputeVisibility model
        model


    let create (initialMap: TileMap) : GameModel =
        createWith 0 initialMap NpcFrames.Default None

    let createAt spawnPointIndex (initialMap: TileMap) : GameModel =
        createWith spawnPointIndex initialMap NpcFrames.Default None

    let createEmpty (initialMap: TileMap) : GameModel =
        create initialMap
 
    let createWithPlayer (initialMap: TileMap) (playerFrames: NpcFrames) (playerActorId: int) : GameModel =
        createWith 0 initialMap playerFrames (Some playerActorId) 

    let setVisibilityWindow (model: GameModel) (halfWidth: int) (halfHeight: int) : unit =
        model.VisibilityHalfWidth <- max 0 halfWidth
        model.VisibilityHalfHeight <- max 0 halfHeight
        recomputeVisibility model

    let setVisibilityTranslucencyBudget (model: GameModel) (budget: int) : unit =
        model.VisibilityTranslucencyBudget <- max 0 budget
        recomputeVisibility model
        
    let lookAt (model: GameModel) (pos: GridPos) : LookAtInfo =
        let map = model.Map
        if not (Utils.inBounds map pos) then
            { Result = LookAtResult.Null
              LayerCount = 0uy
              HasMore = false
              LayerKey0 = ""
              LayerKey1 = ""
              LayerKey2 = ""
              BaseKey = "" }
        else 
            // Keep only the top 3 by (RenderLayer, StableOrder).
            // Highest RenderLayer draws last (top). For ties, higher StableOrder draws last (top).
            let mutable e0Id = -1
            let mutable e0Layer = System.Int32.MinValue
            let mutable e0Order = System.Int32.MinValue

            let mutable e1Id = -1
            let mutable e1Layer = System.Int32.MinValue
            let mutable e1Order = System.Int32.MinValue

            let mutable e2Id = -1
            let mutable e2Layer = System.Int32.MinValue
            let mutable e2Order = System.Int32.MinValue

            let inline better (layerA: int) (orderA: int) (layerB: int) (orderB: int) =
                layerA > layerB || (layerA = layerB && orderA > orderB)

            let inline pushCandidate (id: int) (stableOrder: int) =
                if id < 0 then ()
                else
                    let layer = Utils.tryGetRenderLayer id
                    if better layer stableOrder e0Layer e0Order then
                        e2Id <- e1Id; e2Layer <- e1Layer; e2Order <- e1Order
                        e1Id <- e0Id; e1Layer <- e0Layer; e1Order <- e0Order
                        e0Id <- id;   e0Layer <- layer;    e0Order <- stableOrder
                    elif better layer stableOrder e1Layer e1Order then
                        e2Id <- e1Id; e2Layer <- e1Layer; e2Order <- e1Order
                        e1Id <- id;   e1Layer <- layer;    e1Order <- stableOrder
                    elif better layer stableOrder e2Layer e2Order then
                        e2Id <- id;   e2Layer <- layer;    e2Order <- stableOrder

            let cell = map.GetLayerCell(pos.X, pos.Y)
            let baseKey =
                match map.TryGetTileDescriptionKey(pos.X, pos.Y) with
                | None -> ""
                | Some k -> k

            // Gather candidates with stableOrder similar to rendering code.
            // This makes lookAt reflect what's visually on top.
            let mutable stableOrder = 0

            match cell.FixtureId with
            | Some fid ->
                pushCandidate fid stableOrder
                stableOrder <- stableOrder + 1
            | None -> ()

            match cell.ActorId with
            | Some aid ->
                pushCandidate aid stableOrder
                stableOrder <- stableOrder + 1
            | None -> ()

            // Items can be many; only consider the top render slice (no allocations).
            let view = LayerQueries.GetRenderItemView cell
            let mutable it = view.Start
            let itEnd = view.Start + view.Count
            while it < itEnd do
                pushCandidate view.Items.[it] stableOrder
                stableOrder <- stableOrder + 1
                it <- it + 1

            // Decals are bottom->top; stableOrder preserves insertion order as a tiebreaker.
            let mutable di = 0
            while di < cell.DecalCount do
                pushCandidate cell.Decals.[di] stableOrder
                stableOrder <- stableOrder + 1
                di <- di + 1

            let k0 = if e0Id >= 0 then Utils.tryGetEntityDescKey e0Id else ""
            let k1 = if e1Id >= 0 then Utils.tryGetEntityDescKey e1Id else ""
            let k2 = if e2Id >= 0 then Utils.tryGetEntityDescKey e2Id else ""

            let count =
                (if System.String.IsNullOrEmpty(k0) then 0 else 1)
                + (if System.String.IsNullOrEmpty(k1) then 0 else 1)
                + (if System.String.IsNullOrEmpty(k2) then 0 else 1)

            // "HasMore" means there are more layer objects present than listed.
            // (We intentionally base this on presence, not on whether DescKey exists.)
            let totalLayerObjects =
                (if cell.FixtureId.IsSome then 1 else 0)
                + (if cell.ActorId.IsSome then 1 else 0)
                + cell.Items.Count
                + cell.DecalCount

            let hasMore = totalLayerObjects > count

            let result =
                if count = 0 && System.String.IsNullOrEmpty(baseKey) then LookAtResult.Null
                elif count = 0 then LookAtResult.SingleObject
                elif hasMore then LookAtResult.MultipleObjectsIncompleteList
                else LookAtResult.MultipleObjectsFullyListed

            { Result = result
              LayerCount = byte count
              HasMore = hasMore
              LayerKey0 = k0
              LayerKey1 = k1
              LayerKey2 = k2
              BaseKey = baseKey }

    let interactAt (model: GameModel) (pos: GridPos) (interaction: InteractionType)  =
        // Keep this thin: resolve what we hit, then delegate.
        let map = model.Map 
        match interaction with
        | InteractionType.NullAction -> Nothing
        | InteractionType.DoorInteraction doorAction -> Doors.tryInteractDoor model pos doorAction
 
