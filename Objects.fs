namespace AspectGameEngine

open System.Collections.Generic 

module ObjectMovement =
    // Local dictionary to track moveable objects by position.
    // The value is the required strength to move the object.
    // This is a stub: in a full implementation, this would be populated
    // from game data or runtime state.
    let private moveableObjects = Dictionary<GridPos, int>()

    /// Checks if the player meets the strength requirements to move an object at the given position.
    /// First checks the local dictionary, then falls back to the fixture's Moveable property.
    /// Returns true if the object can be moved by the player.
    let canPlayerMoveObject (playerStrength: int) (map: TileMap) (pos: GridPos) : bool =
        // Check local dictionary first (stub priority)
        match moveableObjects.TryGetValue(pos) with
        | true, requiredStrength -> 
            playerStrength >= requiredStrength
        | false, _ ->
            // Fall back to checking the fixture at this position
            match map.TryGetFixture(pos.X, pos.Y) with
            | Some fixtureId ->
                match EntityRegistry.SpriteProps.TryGetValue(fixtureId) with
                | true, spriteProps ->
                    match spriteProps.SpriteType with
                    | SpriteType.Fixture fixtureProps ->
                        // If Moveable is 0, it's unmoveable. If > 0, compare with player strength.
                        fixtureProps.Moveable > 0 && playerStrength >= fixtureProps.Moveable
                    | _ -> false
                | false, _ -> false
            | None -> false

    /// Determines if the player can move to the position resulting from applying the delta to the current position.
    /// Checks that the new position is within bounds, walkable, and not occupied by an actor or blocking fixture.
    /// This is optimized to minimize lookups by using existing TileMap methods.
    let canMoveTo (map: TileMap) (current: GridPos) (delta: GridDelta) : bool =
        let newX = current.X + delta.DX
        let newY = current.Y + delta.DY
        map.IsWalkable(newX, newY) && not (map.IsOccupied(newX, newY))

    /// Attempts to resolve a move into a moveable fixture.
    /// Returns true if the fixture was pushed forward or swapped with the player.
    /// 
    /// Movement rules:
    /// - The fixture is at (playerPos + delta) - the tile player is trying to enter
    /// - First try moving fixture to (fixturePos + delta) - 1 tile further in same direction
    /// - If blocked, swap the player actor and fixture
    /// - Only pushes if destination is walkable and unoccupied
    /// - Only attempts push if fixture is moveable by player's strength
    let tryPushFixture (playerPos: GridPos) (delta: GridDelta) (map: TileMap) (playerStrength: int) : bool =
        // Calculate fixture position (the tile player is trying to enter)
        let fixtureX = playerPos.X + delta.DX
        let fixtureY = playerPos.Y + delta.DY
        
        // Check if there's a moveable fixture at that position
        if not (canPlayerMoveObject playerStrength map (GridPos(fixtureX, fixtureY))) then
            false
        else
            // First attempt: push in same direction (fixture moves 1 tile further)
            let pushX = fixtureX + delta.DX
            let pushY = fixtureY + delta.DY
            
            if map.IsWalkable(pushX, pushY) && not (map.IsOccupied(pushX, pushY)) then
                map.TryMoveFixture(fixtureX, fixtureY, pushX, pushY)
            else
                map.TrySwapActorAndFixture(playerPos.X, playerPos.Y, fixtureX, fixtureY)
