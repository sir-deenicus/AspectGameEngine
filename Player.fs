namespace AspectGameEngine

module Player = 
    let private defaultPlayerStrength = 1

    [<Struct>]
    type private OpacitySnapshot =
        { Position: GridPos
          Opacity: TileOpacity }

    let private captureOpacities (map: TileMap) (positions: GridPos[]) =
        positions
        |> Array.map (fun pos -> { Position = pos; Opacity = map.GetOpacity(pos.X, pos.Y) })

    let private opacitiesChanged (map: TileMap) (snapshots: OpacitySnapshot[]) =
        snapshots
        |> Array.exists (fun snapshot -> map.GetOpacity(snapshot.Position.X, snapshot.Position.Y) <> snapshot.Opacity)

    let private movementMessage key args =
        Some { Key = key; Args = args }

    let private posArgs prefix (pos: GridPos) =
        [| prefix + ".x", EngineMessageArg.Int pos.X
           prefix + ".y", EngineMessageArg.Int pos.Y |]

    let private blockedMessage cause =
        let key =
            match cause with
            | MovementBlockedCause.NoMovement -> "movement.blocked.no-movement"
            | MovementBlockedCause.MissingPlayerActor -> "movement.blocked.missing-player-actor"
            | MovementBlockedCause.DestinationOutOfBounds -> "movement.blocked.destination-out-of-bounds"
            | MovementBlockedCause.DestinationNotWalkable -> "movement.blocked.destination-not-walkable"
            | MovementBlockedCause.DestinationOccupiedByActor -> "movement.blocked.destination-occupied-by-actor"
            | MovementBlockedCause.DestinationBlockedByFixture -> "movement.blocked.destination-blocked-by-fixture"
            | MovementBlockedCause.MoveableRequiresStrength -> "movement.blocked.moveable-requires-strength"
            | MovementBlockedCause.MoveablePushDestinationOutOfBounds -> "movement.blocked.moveable-push-destination-out-of-bounds"
            | MovementBlockedCause.MoveablePushDestinationBlocked -> "movement.blocked.moveable-push-destination-blocked"
            | MovementBlockedCause.SwapBlocked -> "movement.blocked.swap-blocked"
            | MovementBlockedCause.PlayerActorNotAtPosition -> "movement.blocked.player-actor-not-at-position"
            | _ -> "movement.blocked.unknown"
        movementMessage key [||]

    let private normalMoveMessage oldPos newPos =
        movementMessage "movement.moved" (Array.append (posArgs "from" oldPos) (posArgs "to" newPos))

    let private moveableMoveMessage key oldPos newPos fixtureId fixtureOldPos fixtureNewPos =
        [| yield! posArgs "player.from" oldPos
           yield! posArgs "player.to" newPos
           "fixture.id", EngineMessageArg.Int fixtureId
           yield! posArgs "fixture.from" fixtureOldPos
           yield! posArgs "fixture.to" fixtureNewPos |]
        |> movementMessage key

    let private resolvePlayerSprite (frames: NpcFrames) (visual: PlayerVisualState) : SpriteRef =
        match visual.State, visual.Facing with
        | ActorPose.Attack, ActorFacing.Right -> frames.AttackRight
        | ActorPose.Attack, _ -> frames.AttackLeft
        | _, ActorFacing.Right -> frames.NormalRight
        | _ -> frames.NormalLeft

    let syncVisualToRender (model: GameModel) : unit =
        match model.PlayerModel.PlayerActorId with
        | None -> ()
        | Some actorId ->
            match SpritePropsQueries.tryGet actorId with
            | None -> ()
            | Some sp ->
                let spriteRef = resolvePlayerSprite model.PlayerModel.PlayerFrames model.PlayerModel.PlayerVisual
                EntityRegistry.SpriteProps.[actorId] <- { sp with Sprite = spriteRef }

    let private blockedMovement oldPos cause =
        { Succeeded = false
          Kind = MovementKind.Normal
          BlockedCause = Some cause
          Message = blockedMessage cause
          PlayerOldPosition = oldPos
          PlayerNewPosition = oldPos
          MovedObject = None
          Changes = EngineChangeSet.Empty }

    let private successfulMovement oldPos newPos actorId doorOpened occlusionInputChanged =
        let changedBaseCells =
            if doorOpened then [| newPos |]
            else [||]

        { Succeeded = true
          Kind = MovementKind.Normal
          BlockedCause = None
          Message = normalMoveMessage oldPos newPos
          PlayerOldPosition = oldPos
          PlayerNewPosition = newPos
          MovedObject = None
          Changes =
            { ChangedBaseCells = changedBaseCells
              ChangedLayerCells = [| oldPos; newPos |]
              ChangedEntities =
                [| { Position = oldPos
                     Slot = ChangeSlot.Actor
                     EntityId = None
                     LocalObjectId = None }
                   { Position = newPos
                     Slot = ChangeSlot.Actor
                     EntityId = Some actorId
                     LocalObjectId = None } |]
              VisibilityInputChanged = true
              OcclusionInputChanged = occlusionInputChanged
              SaveRelevant = true } }

    let private successfulMoveableMovement oldPos newPos actorId doorOpened kind fixtureId fixtureOldPos fixtureNewPos changedLayerCells occlusionInputChanged =
        let changedBaseCells =
            if doorOpened then [| newPos |]
            else [||]

        { Succeeded = true
          Kind = kind
          BlockedCause = None
          Message =
            let key =
                match kind with
                | MovementKind.PushedMoveable -> "movement.moveable.pushed"
                | MovementKind.SwappedMoveable -> "movement.moveable.swapped"
                | _ -> "movement.moveable.moved"
            moveableMoveMessage key oldPos newPos fixtureId fixtureOldPos fixtureNewPos
          PlayerOldPosition = oldPos
          PlayerNewPosition = newPos
          MovedObject =
            Some
                { Slot = ChangeSlot.Fixture
                  EntityId = Some fixtureId
                  LocalObjectId = None
                  OldPosition = fixtureOldPos
                  NewPosition = fixtureNewPos }
          Changes =
            { ChangedBaseCells = changedBaseCells
              ChangedLayerCells = changedLayerCells
              ChangedEntities =
                [| { Position = oldPos
                     Slot = ChangeSlot.Actor
                     EntityId = None
                     LocalObjectId = None }
                   { Position = newPos
                     Slot = ChangeSlot.Actor
                     EntityId = Some actorId
                     LocalObjectId = None }
                   { Position = fixtureOldPos
                     Slot = ChangeSlot.Fixture
                     EntityId = None
                     LocalObjectId = None }
                   { Position = fixtureNewPos
                     Slot = ChangeSlot.Fixture
                     EntityId = Some fixtureId
                     LocalObjectId = None } |]
              VisibilityInputChanged = true
              OcclusionInputChanged = occlusionInputChanged
              SaveRelevant = true } }

    let private fixtureMoveRequirement fixtureId =
        match EntityRegistry.SpriteProps.TryGetValue(fixtureId) with
        | true, spriteProps ->
            match spriteProps.SpriteType with
            | SpriteType.Fixture fixtureProps -> Some fixtureProps.Moveable
            | _ -> None
        | false, _ -> None

    let private fixtureBlocksMovement fixtureId =
        match EntityRegistry.SpriteProps.TryGetValue(fixtureId) with
        | true, spriteProps -> SpritePropsQueries.checkFixtureBlocksMovement spriteProps.SpriteType
        | false, _ -> true

    let private finishPlayerMove (model: GameModel) (oldPos: GridPos) (newPos: GridPos) dx actorId =
        model.PlayerModel.PlayerPos <- newPos

        let doorOpened = Doors.tryAutoOpenDoor model newPos

        let newFacing =
            if dx > 0 then ActorFacing.Right
            elif dx < 0 then ActorFacing.Left 
            else model.PlayerModel.PlayerVisual.Facing
        model.PlayerModel.PlayerVisual.Facing <- newFacing
        syncVisualToRender model
        GameUpdate.recomputeVisibility model
        doorOpened

    let private tryMoveIntoMoveableFixture (model: GameModel) (oldPos: GridPos) (newPos: GridPos) (delta: GridDelta) actorId fixtureId =
        let map = model.Map
        match fixtureMoveRequirement fixtureId with
        | None ->
            blockedMovement oldPos MovementBlockedCause.DestinationBlockedByFixture
        | Some requirement when requirement <= 0 ->
            blockedMovement oldPos MovementBlockedCause.DestinationBlockedByFixture
        | Some requirement when requirement > defaultPlayerStrength ->
            blockedMovement oldPos MovementBlockedCause.MoveableRequiresStrength
        | Some _ ->
            let pushPos = GridPos(newPos.X + delta.DX, newPos.Y + delta.DY)
            let pushOpacitySnapshots = captureOpacities map [| oldPos; newPos; pushPos |]
            if map.TryPushFixtureAndMoveActor(oldPos.X, oldPos.Y, newPos.X, newPos.Y, pushPos.X, pushPos.Y) then
                let doorOpened = finishPlayerMove model oldPos newPos delta.DX actorId
                let occlusionInputChanged = doorOpened || opacitiesChanged map pushOpacitySnapshots
                successfulMoveableMovement
                    oldPos
                    newPos
                    actorId
                    doorOpened
                    MovementKind.PushedMoveable
                    fixtureId
                    newPos
                    pushPos
                    [| oldPos; newPos; pushPos |]
                    occlusionInputChanged
            else
                let swapOpacitySnapshots = captureOpacities map [| oldPos; newPos |]
                if map.TrySwapActorAndFixture(oldPos.X, oldPos.Y, newPos.X, newPos.Y) then
                    let doorOpened = finishPlayerMove model oldPos newPos delta.DX actorId
                    let occlusionInputChanged = doorOpened || opacitiesChanged map swapOpacitySnapshots
                    successfulMoveableMovement
                        oldPos
                        newPos
                        actorId
                        doorOpened
                        MovementKind.SwappedMoveable
                        fixtureId
                        newPos
                        oldPos
                        [| oldPos; newPos |]
                        occlusionInputChanged
                else
                    blockedMovement oldPos MovementBlockedCause.SwapBlocked

    let tryMove(model: GameModel) (dx: int) (dy: int) : MovementResult =
        let map = model.Map
        let oldPos = model.PlayerModel.PlayerPos

        if dx = 0 && dy = 0 then blockedMovement oldPos MovementBlockedCause.NoMovement
        else
            let nx = oldPos.X + dx
            let ny = oldPos.Y + dy
            let newPos = GridPos(nx, ny)

            match model.PlayerModel.PlayerActorId with
            | None -> blockedMovement oldPos MovementBlockedCause.MissingPlayerActor
            | Some actorId ->
                if nx < 0 || nx >= map.Width || ny < 0 || ny >= map.Height then
                    blockedMovement oldPos MovementBlockedCause.DestinationOutOfBounds
                elif
                    match map.TryGetActor(oldPos.X, oldPos.Y) with
                    | Some sourceActorId -> sourceActorId <> actorId
                    | None -> true
                then
                    blockedMovement oldPos MovementBlockedCause.PlayerActorNotAtPosition
                elif not (map.IsWalkable(nx, ny)) then
                    blockedMovement oldPos MovementBlockedCause.DestinationNotWalkable
                elif map.TryGetActor(nx, ny).IsSome then
                    blockedMovement oldPos MovementBlockedCause.DestinationOccupiedByActor
                elif map.TryGetFixture(nx, ny) |> Option.exists fixtureBlocksMovement then
                    match map.TryGetFixture(nx, ny) with
                    | Some fixtureId ->
                        tryMoveIntoMoveableFixture model oldPos newPos (GridDelta(dx, dy)) actorId fixtureId
                    | None ->
                        blockedMovement oldPos MovementBlockedCause.DestinationBlockedByFixture
                else
                    // Direct path: for player-actor movement, delegate to TileMap.TryMoveActor.
                    // Doors are not path-blocking, so opening should run after movement.
                    let opacitySnapshots = captureOpacities map [| oldPos; newPos |]
                    if map.TryMoveActor(oldPos.X, oldPos.Y, nx, ny) then
                        let doorOpened = finishPlayerMove model oldPos newPos dx actorId
                        let occlusionInputChanged = doorOpened || opacitiesChanged map opacitySnapshots

                        successfulMovement oldPos newPos actorId doorOpened occlusionInputChanged
                    else
                        blockedMovement oldPos MovementBlockedCause.PlayerActorNotAtPosition

    let tryMoveBool(model: GameModel) (dx: int) (dy: int) : bool =
        (tryMove model dx dy).ToBool()

    let setFacing (model: GameModel) (facing: ActorFacing) =
        model.PlayerModel.PlayerVisual.Facing <- facing
        syncVisualToRender model

    let setPose (model: GameModel) (pose: ActorPose) =
        model.PlayerModel.PlayerVisual.State <- pose
        syncVisualToRender model
