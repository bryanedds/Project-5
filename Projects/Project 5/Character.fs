namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacterState world : CharacterState = this.Get (nameof this.CharacterState) world
        member this.SetCharacterState (value : CharacterState) world = this.Set (nameof this.CharacterState) value world
        member this.CharacterState = lens (nameof this.CharacterState) this this.GetCharacterState this.SetCharacterState
        member this.GetCharacterType world : CharacterType = this.Get (nameof this.CharacterType) world
        member this.SetCharacterType (value : CharacterType) world = this.Set (nameof this.CharacterType) value world
        member this.CharacterType = lens (nameof this.CharacterType) this this.GetCharacterType this.SetCharacterType
        member this.GetActionState world : ActionState = this.Get (nameof this.ActionState) world
        member this.SetActionState (value : ActionState) world = this.Set (nameof this.ActionState) value world
        member this.ActionState = lens (nameof this.ActionState) this this.GetActionState this.SetActionState
        member this.GetMovementState world : MovementState = this.Get (nameof this.MovementState) world
        member this.SetMovementState (value : MovementState) world = this.Set (nameof this.MovementState) value world
        member this.MovementState = lens (nameof this.MovementState) this this.GetMovementState this.SetMovementState
        member this.GetHitPoints world : int = this.Get (nameof this.HitPoints) world
        member this.SetHitPoints (value : int) world = this.Set (nameof this.HitPoints) value world
        member this.HitPoints = lens (nameof this.HitPoints) this this.GetHitPoints this.SetHitPoints
        member this.GetInsertionSpotCollisions world : Entity Set = this.Get (nameof this.InsertionSpotCollisions) world
        member this.SetInsertionSpotCollisions (value : Entity Set) world = this.Set (nameof this.InsertionSpotCollisions) value world
        member this.InsertionSpotCollisions = lens (nameof this.InsertionSpotCollisions) this this.GetInsertionSpotCollisions this.SetInsertionSpotCollisions
        member this.GetDoorSpotCollisions world : Entity Set = this.Get (nameof this.DoorSpotCollisions) world
        member this.SetDoorSpotCollisions (value : Entity Set) world = this.Set (nameof this.DoorSpotCollisions) value world
        member this.DoorSpotCollisions = lens (nameof this.DoorSpotCollisions) this this.GetDoorSpotCollisions this.SetDoorSpotCollisions
        member this.GetInvestigationSpotCollisions world : Entity Set = this.Get (nameof this.InvestigationSpotCollisions) world
        member this.SetInvestigationSpotCollisions (value : Entity Set) world = this.Set (nameof this.InvestigationSpotCollisions) value world
        member this.InvestigationSpotCollisions = lens (nameof this.InvestigationSpotCollisions) this this.GetInvestigationSpotCollisions this.SetInvestigationSpotCollisions
        member this.GetHidingSpotCollisions world : Entity Set = this.Get (nameof this.HidingSpotCollisions) world
        member this.SetHidingSpotCollisions (value : Entity Set) world = this.Set (nameof this.HidingSpotCollisions) value world
        member this.HidingSpotCollisions = lens (nameof this.HidingSpotCollisions) this this.GetHidingSpotCollisions this.SetHidingSpotCollisions
        member this.GetWeaponCollisions world : Entity Set = this.Get (nameof this.WeaponCollisions) world
        member this.SetWeaponCollisions (value : Entity Set) world = this.Set (nameof this.WeaponCollisions) value world
        member this.WeaponCollisions = lens (nameof this.WeaponCollisions) this this.GetWeaponCollisions this.SetWeaponCollisions
        member this.GetWeaponModel world : StaticModel AssetTag = this.Get (nameof this.WeaponModel) world
        member this.SetWeaponModel (value : StaticModel AssetTag) world = this.Set (nameof this.WeaponModel) value world
        member this.WeaponModel = lens (nameof this.WeaponModel) this this.GetWeaponModel this.SetWeaponModel
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DeathEvent = Events.DeathEvent --> this

type CharacterDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static let processEnemyNavigation (goalPosition : Vector3) (entity : Entity) world =
        let navSpeedsOpt =
            match entity.GetActionState world with
            | NormalState ->
                let characterType = entity.GetCharacterType world
                Some (characterType.WalkSpeed, characterType.TurnSpeed)
            | _ -> None
        match navSpeedsOpt with
        | Some (moveSpeed, turnSpeed) ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let followOutput = World.nav3dFollow None None moveSpeed turnSpeed position rotation goalPosition Simulants.Gameplay world    
            let world = entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
            let world = entity.SetRotation followOutput.NavRotation world
            world
        | None -> world

    static let processEnemyAggression (targetPosition : Vector3) targetBodyIds (entity : Entity) world =

        // attacking
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let position = entity.GetPosition world
                let positionFlat = position.WithY 0.0f
                let rotation = entity.GetRotation world
                let bodyId = entity.GetBodyId world
                if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId targetBodyIds world then
                    let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
                    let playerPositionFlat = targetPosition.WithY 0.0f
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.0f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                        let world = entity.SetActionState (AttackState (AttackState.make world.GameTime)) world
                        entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                    else world
                else world
            | _ -> world

        // navigation
        let world =
            let navSpeedsOpt =
                match entity.GetActionState world with
                | NormalState ->
                    let characterType = entity.GetCharacterType world
                    Some (characterType.WalkSpeed, characterType.TurnSpeed)
                | _ -> None
            match navSpeedsOpt with
            | Some (moveSpeed, turnSpeed) ->
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let sphere =
                    if position.Y - targetPosition.Y >= 0.25f
                    then Sphere (targetPosition, 0.1f) // when above player
                    else Sphere (targetPosition, 0.4f) // when at or below player
                let nearest = sphere.Nearest position
                let followOutput = World.nav3dFollow (Some Constants.Gameplay.AttackProximity) None moveSpeed turnSpeed position rotation nearest Simulants.Gameplay world    
                let world = entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
                let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
                let world = entity.SetRotation followOutput.NavRotation world
                world
            | None -> world

        // fin
        world

    static let processEnemyUncovering (targetPosition : Vector3) (entity : Entity) world =

        // opening door
        let (uncovered, world) =
            match entity.GetActionState world with
            | NormalState ->
                let doorSpotCollisions = entity.GetDoorSpotCollisions world
                match Seq.tryHead doorSpotCollisions with
                | Some doorSpot ->
                    match doorSpot.GetDoorState world with
                    | DoorClosed ->
                        let world = doorSpot.SetDoorState (DoorOpening world.GameTime) world
                        (true, world)
                    | _ -> (false, world)
                | None -> (false, world)
            | _ -> (false, world)

        // navigation
        let world =
            let navSpeedsOpt =
                match entity.GetActionState world with
                | NormalState ->
                    let characterType = entity.GetCharacterType world
                    Some (characterType.WalkSpeed, characterType.TurnSpeed)
                | _ -> None
            match navSpeedsOpt with
            | Some (moveSpeed, turnSpeed) ->
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let sphere =
                    if position.Y - targetPosition.Y >= 0.25f
                    then Sphere (targetPosition, 0.1f) // when above player
                    else Sphere (targetPosition, 0.4f) // when at or below player
                let nearest = sphere.Nearest position
                let followOutput = World.nav3dFollow (Some Constants.Gameplay.AttackProximity) None moveSpeed turnSpeed position rotation nearest Simulants.Gameplay world    
                let world = entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
                let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
                let world = entity.SetRotation followOutput.NavRotation world
                world
            | None -> world

        // fin
        (uncovered, world)

    static let processHunterWayPointNavigation (state : HunterState) (entity : Entity) world =
        match state.HunterWayPoints with
        | [||] -> world
        | wayPoints ->
            match state.HunterWayPointIndexOpt with
            | Some wayPointIndex when wayPointIndex < wayPoints.Length ->
                let wayPoint = wayPoints.[wayPointIndex]
                match tryResolve entity wayPoint.WayPoint with
                | Some wayPointEntity ->
                    let wayPointPosition = wayPointEntity.GetPosition world
                    let wayPointDistance = Vector3.Distance (entity.GetPosition world, wayPointPosition)
                    if wayPointDistance < Constants.Gameplay.HuntWayPointProximity then
                        match state.HunterWayPointTimeOpt with
                        | None ->
                            let state = { state with HunterWayPointTimeOpt = Some world.GameTime }
                            entity.SetCharacterState (HunterState state) world
                        | Some wayPointTime ->
                            let waitTime = world.GameTime - wayPointTime
                            if waitTime >= wayPoint.WayPointWaitTime then
                                let (wayPointIndexOpt, wayPointBouncing) =
                                    match state.HunterWayPointPlayback with
                                    | Once ->
                                        let wayPointIndex = inc wayPointIndex
                                        if wayPointIndex < wayPoints.Length
                                        then (Some wayPointIndex, false)
                                        else (None, false)
                                    | Loop ->
                                        let wayPointIndex = inc wayPointIndex % wayPoints.Length
                                        (Some wayPointIndex, false)
                                    | Bounce ->
                                        if not state.HunterWayPointBouncing then
                                            let wayPointIndex = inc wayPointIndex
                                            if wayPointIndex = wayPoints.Length
                                            then (Some (dec wayPointIndex), true)
                                            else (Some wayPointIndex, false)
                                        else
                                            let wayPointIndex = dec wayPointIndex
                                            if wayPointIndex < 0
                                            then (Some (inc wayPointIndex), false)
                                            else (Some wayPointIndex, true)
                                let state =
                                    { state with
                                        HunterWayPointBouncing = wayPointBouncing
                                        HunterWayPointIndexOpt = wayPointIndexOpt
                                        HunterWayPointTimeOpt = None }
                                entity.SetCharacterState (HunterState state) world
                            else
                                let world = entity.LinearVelocity.Map ((*) 0.5f) world
                                let world = entity.AngularVelocity.Map ((*) 0.5f) world
                                world
                    else processEnemyNavigation wayPointPosition entity world
                | None -> world
            | Some _ | None ->
                let world = entity.LinearVelocity.Map ((*) 0.5f) world
                let world = entity.AngularVelocity.Map ((*) 0.5f) world
                world

    static let processHunterState targetPosition targetBodyIds targetActionState (state : HunterState) (entity : Entity) (world : World) =

        // process target sighting
        let position = entity.GetPosition world
        let rotation = entity.GetRotation world
        let bodyId = entity.GetBodyId world
        let state =
            if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId targetBodyIds world then
                match targetActionState with
                | HideState hide ->
                    match hide.HidePhase with
                    | HideEntering -> { state with HunterAwareness = AwareOfTargetHiding world.GameTime }
                    | HideWaiting -> state
                    | HideEmerging -> { state with HunterAwareness = AwareOfTargetTraversing world.GameTime }
                    | HideUncovered -> state
                | _ -> { state with HunterAwareness = AwareOfTargetTraversing world.GameTime }
            else state
        let world = entity.SetCharacterState (HunterState state) world

        // process hunter state
        match state.HunterAwareness with
        | UnawareOfTarget ->
            (false, processHunterWayPointNavigation state entity world)
        | AwareOfTargetTraversing startTime ->
            let awareProgress = GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetTraversingDuration
            if awareProgress = 1.0f then
                let state = { state with HunterAwareness = UnawareOfTarget }
                (false, entity.SetCharacterState (HunterState state) world)
            else (false, processEnemyAggression targetPosition targetBodyIds entity world)
        | AwareOfTargetHiding startTime ->
            let awareProgress = GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetHidingDuration
            if awareProgress = 1.0f then
                let state = { state with HunterAwareness = UnawareOfTarget }
                (false, entity.SetCharacterState (HunterState state) world)
            else
                let (uncovered, world) = processEnemyUncovering targetPosition entity world
                if uncovered then
                    let state = { state with HunterAwareness = AwareOfTargetTraversing world.GameTime }
                    let world = entity.SetCharacterState (HunterState state) world
                    (true, world)
                else (false, world)

    static let processStalkerState targetPosition targetBodyIds targetActionState (state : StalkerState) (entity : Entity) world =
        match state with
        | IdlingState -> world
        | StalkingState stalking ->
            if not stalking.Awareness.IsUnawareOfTarget
            then processEnemyAggression targetPosition targetBodyIds entity world
            else world
        | LeavingState leaving -> processEnemyNavigation leaving.UnspawnPosition entity world

    static let processPlayerInput (state : PlayerState) (entity : Entity) world =

        // action
        let world =
        
            // attacking
            if World.isKeyboardKeyPressed KeyboardKey.RShift world && false then
                match entity.GetActionState world with
                | NormalState ->
                    let world = entity.SetActionState (AttackState (AttackState.make world.GameTime)) world
                    entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                | _ -> world
        
            // do nothing
            else world

        // movement
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let rotation = entity.GetRotation world
                let characterType = entity.GetCharacterType world
                let walkSpeed = characterType.WalkSpeed
                let forward = rotation.Forward
                let right = rotation.Right
                let walkDirection =
                    (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right else v3Zero)
                let walkVelocity = if walkDirection <> v3Zero then walkDirection.Normalized * walkSpeed else v3Zero
                let world = entity.SetLinearVelocity (walkVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
                world
            | _ -> world

        // rotation
        let world =
            match entity.GetActionState world with
            | NormalState | InventoryState | InsertionState _  | InvestigationState _ | HideState _ ->
                let rotation = entity.GetRotation world
                let characterType = entity.GetCharacterType world
                let turnSpeed = characterType.TurnSpeed
                let turnVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity * world.GameDelta.Seconds) else rotation
                let world = entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
                let world = entity.SetRotation rotation world
                world
            | _ -> world

        // toggle view flip
        let world =
            if World.isKeyboardKeyPressed KeyboardKey.Q world then
                entity.SetCharacterState (PlayerState { state with ViewFlip = true }) world
            elif World.isKeyboardKeyPressed KeyboardKey.E world then
                entity.SetCharacterState (PlayerState { state with ViewFlip = false }) world
            else world

        // toggle flash light
        let world =
            if World.isKeyboardKeyPressed KeyboardKey.Space world
            then entity.SetCharacterState (PlayerState { state with FlashLightEnabled = not state.FlashLightEnabled }) world
            else world

        // fin
        world

    static let processPlayerState state entity world =
        processPlayerInput state entity world

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpoledFacet>]

    static member Properties =
        let characterType = Hunter
        [define Entity.Persistent characterType.Persistent
         define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.Static false
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape characterType.BodyShape
         define Entity.Substance (Mass 50.0f)
         define Entity.CharacterProperties characterType.CharacterProperties
         define Entity.CharacterState characterType.InitialState
         define Entity.CharacterType characterType
         define Entity.ActionState NormalState
         define Entity.MovementState (Standing 0.0f)
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.InsertionSpotCollisions Set.empty
         define Entity.DoorSpotCollisions Set.empty
         define Entity.InvestigationSpotCollisions Set.empty
         define Entity.HidingSpotCollisions Set.empty
         define Entity.WeaponCollisions Set.empty
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    override this.Process (entity, world) =

        // process body events
        let characterType = entity.GetCharacterType world
        let (bodyEvents, world) = World.doSubscriptionToBodyEvents "BodyEvents" entity world
        let world =
            FQueue.fold (fun world bodyEvent ->
                match bodyEvent with
                | BodyPenetrationData penetration ->
                    match penetration.BodyShapePenetratee.BodyId.BodySource with
                    | :? Entity as penetratee ->
                        if penetratee.Is<InsertionSpotDispatcher> world then
                            entity.InsertionSpotCollisions.Map (Set.add penetratee) world
                        elif penetratee.Is<DoorSpotDispatcher> world then
                            entity.DoorSpotCollisions.Map (Set.add penetratee) world
                        elif penetratee.Is<InvestigationSpotDispatcher> world then
                            if characterType.IsPlayer
                            then entity.InvestigationSpotCollisions.Map (Set.add penetratee) world
                            else world
                        elif penetratee.Is<HidingSpotDispatcher> world then
                            entity.HidingSpotCollisions.Map (Set.add penetratee) world
                        else world
                    | _ -> world
                | BodySeparationData separation ->
                    match separation with
                    | BodySeparationExplicitData explicit ->
                        match explicit.BodyShapeSeparatee.BodyId.BodySource with
                        | :? Entity as separatee ->
                            let world = entity.InsertionSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.DoorSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.InvestigationSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.HidingSpotCollisions.Map (Set.remove separatee) world
                            world
                        | _ -> world
                    | BodySeparationImplicitData implicit ->
                        match implicit.BodyId.BodySource with
                        | :? Entity as separatee ->
                            let world = entity.InsertionSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.DoorSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.InvestigationSpotCollisions.Map (Set.remove separatee) world
                            let world = entity.HidingSpotCollisions.Map (Set.remove separatee) world
                            world
                        | _ -> world
                | BodyTransformData _ -> world)
                world bodyEvents

        // unmount when advancing to enable physics
        let world =
            if world.Advancing
            then entity.SetMountOptWithAdjustment None world
            else entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world

        // process expanded hide sensor on state
        let world =
            let expandedHideSensorBodyEnabled =
                match entity.GetActionState world with
                | HideState hide -> hide.HidePhase.IsHideEntering
                | _ -> false
            let (_, _, world) =
                World.doSensorModel Constants.Gameplay.CharacterExpandedHideSensorName
                    [Entity.PositionLocal @= entity.GetPosition world
                     Entity.BodyEnabled @= expandedHideSensorBodyEnabled
                     Entity.BodyShape .= characterType.ExpandedHideSensorBodyShape
                     Entity.MountOpt .= None] world
            world

        // process character state
        let world =
            if world.Advancing && Simulants.GameplayPlayer.GetExists world then
                let player = Simulants.GameplayPlayer
                let enemyTargetingEir =
                    let processEnemies =
                        match player.GetActionState world with
                        | InvestigationState investigation -> not (investigation.InvestigationSpot.GetInvestigationPhase world).IsInvestigationFinished
                        | _ -> true
                    if processEnemies then
                        let playerEhs = player / Constants.Gameplay.CharacterExpandedHideSensorName
                        let playerBodyIds = Set.ofList [player.GetBodyId world; playerEhs.GetBodyId world]
                        Right (player.GetPosition world, playerBodyIds, player.GetActionState world)
                    else Left ()
                match entity.GetCharacterState world with
                | HunterState state ->
                    match enemyTargetingEir with
                    | Right (targetPosition, targetBodyIds, targetActionState) ->
                        let (uncoveredPlayer, world) = processHunterState targetPosition targetBodyIds targetActionState state entity world
                        if uncoveredPlayer && player.GetExists world
                        then player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideUncovered }) world
                        else world
                    | Left () -> world
                | StalkerState state ->
                    match enemyTargetingEir with
                    | Right (targetPosition, targetBodyIds, targetActionState) -> processStalkerState targetPosition targetBodyIds targetActionState state entity world
                    | Left () -> world
                | PlayerState state -> processPlayerState state entity world
            else world

        // process action state
        let world =
            if world.Advancing then
                match entity.GetActionState world with
                | NormalState -> world
                | AttackState attack as actionState ->
                    let localTime = world.GameTime - attack.AttackTime
                    let actionState = if localTime <= 0.92f then actionState else NormalState
                    entity.SetActionState actionState world
                | InventoryState -> world
                | InsertionState _ -> world
                | InvestigationState _ -> world
                | HideState hide ->
                    match hide.HidePhase with
                    | HideEntering ->
                        let localTime = world.GameTime - hide.HideTime
                        if localTime >= 1.5f
                        then entity.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideWaiting }) world
                        else world
                    | HideWaiting ->
                        world
                    | HideEmerging ->
                        let localTime = world.GameTime - hide.HideTime
                        if localTime >= 1.5f
                        then entity.SetActionState NormalState world
                        else world
                    | HideUncovered ->
                        world
                | InjuryState injury as actionState ->
                    let localTime = world.GameTime - injury.InjuryTime
                    let injuryTime = characterType.InjuryTime
                    let actionState = if localTime < injuryTime then actionState else NormalState
                    entity.SetActionState actionState world
                | WoundState _ -> world
            else world

        // begin animated model
        let positionInterpolated = entity.GetPositionInterpolated world
        let rotationInterpolated = entity.GetRotationInterpolated world
        let world =
            let actionState = entity.GetActionState world
            let visibilityScalar =
                if characterType.IsPlayer
                then Algorithm.computePlayerVisibilityScalar positionInterpolated rotationInterpolated actionState entity world
                else 1.0f
            World.doAnimatedModel Constants.Gameplay.CharacterAnimatedModelName
                [Entity.Position @= positionInterpolated
                 Entity.Rotation @= rotationInterpolated
                 Entity.Size .= entity.GetSize world
                 Entity.Offset .= entity.GetOffset world
                 Entity.MountOpt .= None
                 Entity.Pickable .= false
                 Entity.AnimatedModel @= characterType.AnimatedModel
                 Entity.MaterialProperties @= { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome (colorOne.WithA visibilityScalar); ScatterTypeOpt = ValueSome SkinScatter }
                 Entity.VisibleLocal @= (visibilityScalar > 0.0f)
                 Entity.RenderStyle @= if visibilityScalar = 1.0f then Deferred else Forward (0.0f, 0.0f)
                 Entity.DualRenderedSurfaceIndices @= if visibilityScalar = 1.0f then Set.singleton 3 else Set.empty
                 Entity.SubsortOffsets @= characterType.SubsortOffsets] world
        let animatedModel = world.DeclaredEntity

        // declare player light
        let world =
            match entity.GetCharacterState world with
            | PlayerState state ->
                World.doLight3d Constants.Gameplay.CharacterLightName
                    [Entity.Position @= positionInterpolated + v3Up * 1.2f + rotationInterpolated.Forward * 0.25f
                     Entity.Rotation @= rotationInterpolated * Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2)
                     Entity.MountOpt .= None
                     Entity.Static .= false
                     Entity.LightType .= SpotLight (0.9f, 1.2f)
                     Entity.LightCutoff .= 11.0f
                     Entity.Brightness .= 2.5f
                     Entity.DesireShadows .= true
                     Entity.VisibleLocal @= state.FlashLightEnabled] world
            | _ -> world

        // process traversal animations
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let rotation = entity.GetRotationInterpolated world
                let linearVelocity = entity.GetLinearVelocityInterpolated world
                let angularVelocity = entity.GetAngularVelocityInterpolated world
                let forwardness = linearVelocity.Dot rotation.Forward
                let backness = linearVelocity.Dot -rotation.Forward
                let rightness = linearVelocity.Dot rotation.Right
                let leftness = linearVelocity.Dot -rotation.Right
                let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 0.5f else 0.0f
                let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 0.5f else 0.0f
                let rate = characterType.AnimationRate
                let startTime =
                    match entity.GetMovementState world with
                    | Standing startTime -> startTime
                    | Walking (startTime, _) -> startTime
                let animations =
                    [Animation.make startTime None "Idle" Loop rate 1.0f None]
                let animations =
                    if forwardness >= 0.01f then Animation.make startTime None "WalkForward" Loop rate forwardness None :: animations
                    elif backness >= 0.01f then Animation.make startTime None "WalkBack" Loop rate backness None :: animations
                    else animations
                let animations =
                    if rightness >= 0.01f then Animation.make startTime None "WalkRight" Loop rate rightness None :: animations
                    elif leftness >= 0.01f then Animation.make startTime None "WalkLeft" Loop rate leftness None :: animations
                    else animations
                let animations =
                    if turnRightness >= 0.01f then Animation.make startTime None "TurnRight" Loop rate turnRightness None :: animations
                    elif turnLeftness >= 0.01f then Animation.make startTime None "TurnLeft" Loop rate turnLeftness None :: animations
                    else animations
                let movementState = entity.GetMovementState world
                let world =
                    if List.hasAtLeast 2 animations then
                        if movementState.IsStanding
                        then entity.SetMovementState (Walking (world.GameTime, world.GameTime)) world
                        else world
                    else
                        if movementState.IsWalking
                        then entity.SetMovementState (Standing world.GameTime) world
                        else world
                let world = animatedModel.SetAnimations (Array.ofList animations) world
                world
            | _ -> world

        // process action animations
        let world =
            match entity.GetActionState world with
            | NormalState ->
                match entity.GetMovementState world with
                | Standing _ -> world
                | Walking (startTime, lastStepTime) ->
                    let strideTime = GameTime.ofSeconds 0.75f
                    let offsetTime = GameTime.ofSeconds 0.255f
                    let localStepTime = world.GameTime - lastStepTime + offsetTime
                    if localStepTime >= strideTime then
                        World.playSound 0.25f Assets.Gameplay.StepSound world
                        entity.SetMovementState (Walking (startTime, lastStepTime + strideTime)) world
                    else world
            | AttackState attack ->
                let localTime = world.GameTime - attack.AttackTime
                let world =
                    if localTime > 0.12f && not attack.AttackSoundPlayed then
                        let attack = { attack with AttackSoundPlayed = true }
                        let world = entity.SetActionState (AttackState attack) world
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
                        world
                    else world
                let animation = Animation.once attack.AttackTime None "Attack"
                animatedModel.SetAnimations [|animation|] world
            | InventoryState ->
                world
            | InsertionState insertion ->
                world
            | InvestigationState investigation ->
                world
            | HideState hide ->
                world
            | InjuryState injury ->
                let animation = Animation.once injury.InjuryTime None "WalkBack"
                animatedModel.SetAnimations [|animation|] world
            | WoundState wound ->
                let animation = Animation.loop wound.WoundTime None "WalkBack"
                animatedModel.SetAnimations [|animation|] world

        // declare weapon
        let weaponTransform =
            match animatedModel.TryGetBoneTransformByName Constants.Gameplay.CharacterWeaponHandBoneName world with
            | Some weaponHandBoneTransform ->
                Matrix4x4.CreateTranslation (v3 -0.1f 0.0f 0.02f) *
                Matrix4x4.CreateFromAxisAngle (v3Forward, MathF.PI_OVER_2) *
                weaponHandBoneTransform
            | None -> m4Identity
        let (_, results, world) =
            World.doRigidModel Constants.Gameplay.CharacterWeaponName
                [Entity.Position @= weaponTransform.Translation
                 Entity.Rotation @= weaponTransform.Rotation
                 Entity.Offset .= v3 0.0f 0.5f 0.0f
                 Entity.MountOpt .= None
                 Entity.Visible @= false
                 Entity.Pickable .= false
                 Entity.StaticModel @= entity.GetWeaponModel world
                 Entity.BodyEnabled @= characterType.IsEnemy
                 Entity.BodyType .= Static
                 Entity.BodyShape .= BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor .= true
                 Entity.NavShape .= EmptyNavShape] world

        // process weapon collisions
        let world =
            FQueue.fold (fun world result ->
                match result with
                | BodyPenetrationData penetration ->
                    match penetration.BodyShapePenetratee.BodyId.BodySource with
                    | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                        if characterType.IsPlayer <> (penetratee.GetCharacterType world).IsPlayer
                        then entity.WeaponCollisions.Map (Set.add penetratee) world
                        else world
                    | _ -> world
                | BodySeparationData separation ->
                    match separation with
                    | BodySeparationExplicitData explicit ->
                        match explicit.BodyShapeSeparatee.BodyId.BodySource with
                        | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                            entity.WeaponCollisions.Map (Set.remove separatee) world
                        | _ -> world
                    | BodySeparationImplicitData implicit ->
                        match implicit.BodyId.BodySource with
                        | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                        | _ -> world
                | BodyTransformData _ -> world)
                world results

        // process attacks
        let (attacks, world) =
            match entity.GetActionState world with
            | AttackState attack ->
                let localTime = world.GameTime - attack.AttackTime
                if localTime >= 0.2f && localTime < 0.8f then
                    let weaponCollisions = entity.GetWeaponCollisions world
                    let attacks = Set.difference weaponCollisions attack.AttackedCharacters
                    let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters weaponCollisions }
                    let world = entity.SetActionState (AttackState attack) world
                    (attacks, world)
                else
                    let world = entity.SetActionState (AttackState attack) world
                    (Set.empty, world)
            | _ -> (Set.empty, world)
        let world = Set.fold (fun world attack -> World.publish attack entity.AttackEvent entity world) world attacks

        // declare player hearts
        let world =
            if characterType.IsPlayer then
                let hitPoints = entity.GetHitPoints world
                Seq.fold (fun world i ->
                    World.doStaticSprite ("Heart+" + string i)
                        [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                         Entity.Size .= v3 32.0f 32.0f 0.0f
                         Entity.MountOpt .= None
                         Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty] world)
                    world [0 .. dec characterType.HitPointsMax]
            else world

        // process death
        let world =
            match entity.GetActionState world with
            | WoundState wound when wound.WoundTime >= world.GameTime - GameTime.ofSeconds 1.0f && not wound.WoundEventPublished ->
                let world = World.publish entity entity.DeathEvent entity world
                let wound = { wound with WoundEventPublished = true}
                entity.SetActionState (WoundState wound) world
            | _ -> world

        // fin
        world

    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->
            if (entity.GetCharacterState world).IsEnemyState then
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                for sightRay in Algorithm.computeSightRays Constants.Gameplay.EnemySightDistance position rotation do
                    let segment = Segment3 (sightRay.Origin, sightRay.Origin + sightRay.Direction)
                    World.imGuiSegment3d segment 1.0f Color.Red world
                world
            else world
        | _ -> world

type HunterDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterType = Hunter
        [define Entity.Persistent characterType.Persistent
         define Entity.CharacterState characterType.InitialState
         define Entity.CharacterType characterType
         define Entity.BodyShape characterType.BodyShape
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterProperties characterType.CharacterProperties]

type StalkerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterType = Stalker
        [define Entity.Persistent characterType.Persistent
         define Entity.CharacterState characterType.InitialState
         define Entity.CharacterType characterType
         define Entity.BodyShape characterType.BodyShape
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterProperties characterType.CharacterProperties]

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterType = Player
        [define Entity.Persistent characterType.Persistent
         define Entity.CharacterState characterType.InitialState
         define Entity.CharacterType characterType
         define Entity.BodyShape characterType.BodyShape
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterProperties characterType.CharacterProperties]