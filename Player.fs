namespace AspectGameEngine

module Player = 
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

    let tryMove(model: GameModel) (dx: int) (dy: int) : bool =
        let map = model.Map
        let oldPos = model.PlayerModel.PlayerPos

        if dx = 0 && dy = 0 then false
        else
            let nx = oldPos.X + dx
            let ny = oldPos.Y + dy

            // Direct path: for player-actor movement, delegate to TileMap.TryMoveActor. 
            let moved =
                match model.PlayerModel.PlayerActorId with
                | Some _ -> map.TryMoveActor(oldPos.X, oldPos.Y, nx, ny)
                | None -> false

            if moved then
                model.PlayerModel.PlayerPos <- GridPos(nx, ny)
                
                // Update facing based on movement direction
                let newFacing =
                    if dx > 0 then ActorFacing.Right
                    elif dx < 0 then ActorFacing.Left 
                    else model.PlayerModel.PlayerVisual.Facing
                model.PlayerModel.PlayerVisual.Facing <- newFacing
                syncVisualToRender model
                GameUpdate.recomputeVisibility model

            moved 

    let setFacing (model: GameModel) (facing: ActorFacing) =
        model.PlayerModel.PlayerVisual.Facing <- facing
        syncVisualToRender model

    let setPose (model: GameModel) (pose: ActorPose) =
        model.PlayerModel.PlayerVisual.State <- pose
        syncVisualToRender model
