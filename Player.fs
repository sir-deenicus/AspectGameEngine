namespace AspectGameEngine

module Player = 
    let private defaultPlayerStrength = 1

    let private movementMessage key args =
        Some { Key = key; Args = args }

    let private noMovementCause = Some MovementBlockedCause.NoMovement
    let private missingPlayerActorCause = Some MovementBlockedCause.MissingPlayerActor
    let private destinationOutOfBoundsCause = Some MovementBlockedCause.DestinationOutOfBounds
    let private destinationNotWalkableCause = Some MovementBlockedCause.DestinationNotWalkable
    let private destinationOccupiedByActorCause = Some MovementBlockedCause.DestinationOccupiedByActor
    let private destinationBlockedByFixtureCause = Some MovementBlockedCause.DestinationBlockedByFixture
    let private moveableRequiresStrengthCause = Some MovementBlockedCause.MoveableRequiresStrength
    let private moveablePushDestinationOutOfBoundsCause = Some MovementBlockedCause.MoveablePushDestinationOutOfBounds
    let private moveablePushDestinationBlockedCause = Some MovementBlockedCause.MoveablePushDestinationBlocked
    let private swapBlockedCause = Some MovementBlockedCause.SwapBlocked
    let private playerActorNotAtPositionCause = Some MovementBlockedCause.PlayerActorNotAtPosition

    let private noMovementMessage = movementMessage "movement.blocked.no-movement" [||]
    let private missingPlayerActorMessage = movementMessage "movement.blocked.missing-player-actor" [||]
    let private destinationOutOfBoundsMessage = movementMessage "movement.blocked.destination-out-of-bounds" [||]
    let private destinationNotWalkableMessage = movementMessage "movement.blocked.destination-not-walkable" [||]
    let private destinationOccupiedByActorMessage = movementMessage "movement.blocked.destination-occupied-by-actor" [||]
    let private destinationBlockedByFixtureMessage = movementMessage "movement.blocked.destination-blocked-by-fixture" [||]
    let private moveableRequiresStrengthMessage = movementMessage "movement.blocked.moveable-requires-strength" [||]
    let private moveablePushDestinationOutOfBoundsMessage = movementMessage "movement.blocked.moveable-push-destination-out-of-bounds" [||]
    let private moveablePushDestinationBlockedMessage = movementMessage "movement.blocked.moveable-push-destination-blocked" [||]
    let private swapBlockedMessage = movementMessage "movement.blocked.swap-blocked" [||]
    let private playerActorNotAtPositionMessage = movementMessage "movement.blocked.player-actor-not-at-position" [||]
    let private unknownBlockedMessage = movementMessage "movement.blocked.unknown" [||]

    let private blockedCauseOption cause =
        match cause with
        | MovementBlockedCause.NoMovement -> noMovementCause
        | MovementBlockedCause.MissingPlayerActor -> missingPlayerActorCause
        | MovementBlockedCause.DestinationOutOfBounds -> destinationOutOfBoundsCause
        | MovementBlockedCause.DestinationNotWalkable -> destinationNotWalkableCause
        | MovementBlockedCause.DestinationOccupiedByActor -> destinationOccupiedByActorCause
        | MovementBlockedCause.DestinationBlockedByFixture -> destinationBlockedByFixtureCause
        | MovementBlockedCause.MoveableRequiresStrength -> moveableRequiresStrengthCause
        | MovementBlockedCause.MoveablePushDestinationOutOfBounds -> moveablePushDestinationOutOfBoundsCause
        | MovementBlockedCause.MoveablePushDestinationBlocked -> moveablePushDestinationBlockedCause
        | MovementBlockedCause.SwapBlocked -> swapBlockedCause
        | MovementBlockedCause.PlayerActorNotAtPosition -> playerActorNotAtPositionCause
        | _ -> Some cause

    let private blockedMessage cause =
        match cause with
        | MovementBlockedCause.NoMovement -> noMovementMessage
        | MovementBlockedCause.MissingPlayerActor -> missingPlayerActorMessage
        | MovementBlockedCause.DestinationOutOfBounds -> destinationOutOfBoundsMessage
        | MovementBlockedCause.DestinationNotWalkable -> destinationNotWalkableMessage
        | MovementBlockedCause.DestinationOccupiedByActor -> destinationOccupiedByActorMessage
        | MovementBlockedCause.DestinationBlockedByFixture -> destinationBlockedByFixtureMessage
        | MovementBlockedCause.MoveableRequiresStrength -> moveableRequiresStrengthMessage
        | MovementBlockedCause.MoveablePushDestinationOutOfBounds -> moveablePushDestinationOutOfBoundsMessage
        | MovementBlockedCause.MoveablePushDestinationBlocked -> moveablePushDestinationBlockedMessage
        | MovementBlockedCause.SwapBlocked -> swapBlockedMessage
        | MovementBlockedCause.PlayerActorNotAtPosition -> playerActorNotAtPositionMessage
        | _ -> unknownBlockedMessage

    let private normalMoveMessage (oldPos: GridPos) (newPos: GridPos) =
        movementMessage
            "movement.moved"
            [| "from.x", EngineMessageArg.Int oldPos.X
               "from.y", EngineMessageArg.Int oldPos.Y
               "to.x", EngineMessageArg.Int newPos.X
               "to.y", EngineMessageArg.Int newPos.Y |]

    let private moveableMoveMessage key (oldPos: GridPos) (newPos: GridPos) fixtureId (fixtureOldPos: GridPos) (fixtureNewPos: GridPos) =
        [| "player.from.x", EngineMessageArg.Int oldPos.X
           "player.from.y", EngineMessageArg.Int oldPos.Y
           "player.to.x", EngineMessageArg.Int newPos.X
           "player.to.y", EngineMessageArg.Int newPos.Y
           "fixture.id", EngineMessageArg.Int fixtureId
           "fixture.from.x", EngineMessageArg.Int fixtureOldPos.X
           "fixture.from.y", EngineMessageArg.Int fixtureOldPos.Y
           "fixture.to.x", EngineMessageArg.Int fixtureNewPos.X
           "fixture.to.y", EngineMessageArg.Int fixtureNewPos.Y |]
        |> movementMessage key

    let private resolvePlayerSprite (frames: NpcFrames) (visual: PlayerVisualState) : SpriteRef =
        if visual.State = ActorPose.Attack then
            if visual.Facing = ActorFacing.Right then frames.AttackRight
            else frames.AttackLeft
        elif visual.Facing = ActorFacing.Right then
            frames.NormalRight
        else
            frames.NormalLeft

    let syncVisualToRender (model: GameModel) : unit =
        match model.PlayerModel.PlayerActorId with
        | None -> ()
        | Some actorId ->
            let mutable spriteProps = Unchecked.defaultof<SpriteProperties>
            if EntityRegistry.SpriteProps.TryGetValue(actorId, &spriteProps) then
                let spriteRef = resolvePlayerSprite model.PlayerModel.PlayerFrames model.PlayerModel.PlayerVisual
                EntityRegistry.SpriteProps.[actorId] <- { spriteProps with Sprite = spriteRef }

    let private blockedMovement includeDetails (oldPos: GridPos) cause =
        { Succeeded = false
          Kind = MovementKind.Normal
          BlockedCause = if includeDetails then blockedCauseOption cause else None
          Message = if includeDetails then blockedMessage cause else None
          PlayerOldPosition = oldPos
          PlayerNewPosition = oldPos
          MovedObject = None
          Changes = EngineChangeSet.Empty }

    let private successfulMovement includeDetails (oldPos: GridPos) (newPos: GridPos) actorId doorOpened occlusionInputChanged =
        if not includeDetails then
            { Succeeded = true
              Kind = MovementKind.Normal
              BlockedCause = None
              Message = None
              PlayerOldPosition = oldPos
              PlayerNewPosition = newPos
              MovedObject = None
              Changes = EngineChangeSet.Empty }
        else
            let actorIdOption = Some actorId
            { Succeeded = true
              Kind = MovementKind.Normal
              BlockedCause = None
              Message = normalMoveMessage oldPos newPos
              PlayerOldPosition = oldPos
              PlayerNewPosition = newPos
              MovedObject = None
              Changes =
                { ChangedBaseCells = if doorOpened then [| newPos |] else [||]
                  ChangedLayerCells = [| oldPos; newPos |]
                  ChangedEntities =
                    [| { Position = oldPos
                         Slot = ChangeSlot.Actor
                         EntityId = None
                         LocalObjectId = None }
                       { Position = newPos
                         Slot = ChangeSlot.Actor
                         EntityId = actorIdOption
                         LocalObjectId = None } |]
                  VisibilityInputChanged = true
                  OcclusionInputChanged = occlusionInputChanged
                  SaveRelevant = true } }

    let private successfulMoveableMovement includeDetails (oldPos: GridPos) (newPos: GridPos) actorId doorOpened kind fixtureId (fixtureOldPos: GridPos) (fixtureNewPos: GridPos) occlusionInputChanged =
        if not includeDetails then
            { Succeeded = true
              Kind = kind
              BlockedCause = None
              Message = None
              PlayerOldPosition = oldPos
              PlayerNewPosition = newPos
              MovedObject = None
              Changes = EngineChangeSet.Empty }
        else
            let actorIdOption = Some actorId
            let fixtureIdOption = Some fixtureId
            let messageKey =
                match kind with
                | MovementKind.PushedMoveable -> "movement.moveable.pushed"
                | MovementKind.SwappedMoveable -> "movement.moveable.swapped"
                | _ -> "movement.moveable.moved"
            { Succeeded = true
              Kind = kind
              BlockedCause = None
              Message = moveableMoveMessage messageKey oldPos newPos fixtureId fixtureOldPos fixtureNewPos
              PlayerOldPosition = oldPos
              PlayerNewPosition = newPos
              MovedObject =
                Some
                    { Slot = ChangeSlot.Fixture
                      EntityId = fixtureIdOption
                      LocalObjectId = None
                      OldPosition = fixtureOldPos
                      NewPosition = fixtureNewPos }
              Changes =
                { ChangedBaseCells = if doorOpened then [| newPos |] else [||]
                  ChangedLayerCells =
                    if kind = MovementKind.PushedMoveable then [| oldPos; newPos; fixtureNewPos |]
                    else [| oldPos; newPos |]
                  ChangedEntities =
                    [| { Position = oldPos
                         Slot = ChangeSlot.Actor
                         EntityId = None
                         LocalObjectId = None }
                       { Position = newPos
                         Slot = ChangeSlot.Actor
                         EntityId = actorIdOption
                         LocalObjectId = None }
                       { Position = fixtureOldPos
                         Slot = ChangeSlot.Fixture
                         EntityId = None
                         LocalObjectId = None }
                       { Position = fixtureNewPos
                         Slot = ChangeSlot.Fixture
                         EntityId = fixtureIdOption
                         LocalObjectId = None } |]
                  VisibilityInputChanged = true
                  OcclusionInputChanged = occlusionInputChanged
                  SaveRelevant = true } }

    let private finishPlayerMove (model: GameModel) (newPos: GridPos) dx =
        model.PlayerModel.PlayerPos <- newPos

        let doorOpened = Doors.tryAutoOpenDoor model newPos

        let newFacing =
            if dx > 0 then ActorFacing.Right
            elif dx < 0 then ActorFacing.Left 
            else model.PlayerModel.PlayerVisual.Facing
        model.PlayerModel.PlayerVisual.Facing <- newFacing
        syncVisualToRender model
        doorOpened

    let private tryMoveCore includeDetails (model: GameModel) (dx: int) (dy: int) : MovementResult =
        let map = model.Map
        let oldPos = model.PlayerModel.PlayerPos

        if dx = 0 && dy = 0 then blockedMovement includeDetails oldPos MovementBlockedCause.NoMovement
        else
            let nx = oldPos.X + dx
            let ny = oldPos.Y + dy
            let newPos = GridPos(nx, ny)

            match model.PlayerModel.PlayerActorId with
            | None -> blockedMovement includeDetails oldPos MovementBlockedCause.MissingPlayerActor
            | Some actorId ->
                let transaction =
                    map.TryMoveActorTransaction(
                        actorId,
                        oldPos.X,
                        oldPos.Y,
                        nx,
                        ny,
                        dx,
                        dy,
                        defaultPlayerStrength)

                if not transaction.Succeeded then
                    blockedMovement includeDetails oldPos transaction.BlockedCause
                else
                    let doorOpened = finishPlayerMove model newPos dx
                    let occlusionInputChanged = doorOpened || transaction.OpacityChanged
                    if transaction.Kind = MovementKind.Normal then
                        successfulMovement includeDetails oldPos newPos actorId doorOpened occlusionInputChanged
                    else
                        successfulMoveableMovement
                            includeDetails
                            oldPos
                            newPos
                            actorId
                            doorOpened
                            transaction.Kind
                            transaction.MovedFixtureId
                            transaction.FixtureOldPosition
                            transaction.FixtureNewPosition
                            occlusionInputChanged

    let tryMove(model: GameModel) (dx: int) (dy: int) : MovementResult =
        tryMoveCore true model dx dy

    let tryMoveBool(model: GameModel) (dx: int) (dy: int) : bool =
        (tryMoveCore false model dx dy).ToBool()

    let setFacing (model: GameModel) (facing: ActorFacing) =
        model.PlayerModel.PlayerVisual.Facing <- facing
        syncVisualToRender model

    let setPose (model: GameModel) (pose: ActorPose) =
        model.PlayerModel.PlayerVisual.State <- pose
        syncVisualToRender model
