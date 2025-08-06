namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module HunterDispatcherExtensions =
    type Entity with
        member this.GetHunterState world : HunterState = this.Get (nameof this.HunterState) world
        member this.SetHunterState (value : HunterState) world = this.Set (nameof this.HunterState) value world
        member this.HunterState = lens (nameof this.HunterState) this this.GetHunterState this.SetHunterState

type HunterDispatcher () =
    inherit CharacterDispatcher ()

    static let processHunterWayPointNavigation (entity : Entity) world =
        let state = entity.GetHunterState world
        if Array.notEmpty state.HunterWayPoints then
            match state.HunterWayPointIndexOpt with
            | Some wayPointIndex when wayPointIndex < state.HunterWayPoints.Length ->
                let wayPoint = state.HunterWayPoints.[wayPointIndex]
                match tryResolve entity wayPoint.WayPoint with
                | Some wayPointEntity ->
                    let wayPointPosition = wayPointEntity.GetPosition world
                    let wayPointDistance = wayPointPosition.Distance (entity.GetPosition world)
                    if wayPointDistance < Constants.Gameplay.HuntWayPointProximity then
                        match state.HunterWayPointTimeOpt with
                        | Some wayPointTime ->
                            let waitTime = world.GameTime - wayPointTime
                            if waitTime >= wayPoint.WayPointWaitTime then
                                let (wayPointIndexOpt, wayPointBouncing) =
                                    match state.HunterWayPointPlayback with
                                    | Once ->
                                        let wayPointIndex = inc wayPointIndex
                                        if wayPointIndex < state.HunterWayPoints.Length
                                        then (Some wayPointIndex, false)
                                        else (None, false)
                                    | Loop ->
                                        let wayPointIndex = inc wayPointIndex % state.HunterWayPoints.Length
                                        (Some wayPointIndex, false)
                                    | Bounce ->
                                        if not state.HunterWayPointBouncing then
                                            let wayPointIndex = inc wayPointIndex
                                            if wayPointIndex = state.HunterWayPoints.Length
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
                                entity.SetHunterState state world
                            else
                                entity.LinearVelocity.Map ((*) 0.5f) world
                                entity.AngularVelocity.Map ((*) 0.5f) world
                        | None ->
                            let state = { state with HunterWayPointTimeOpt = Some world.GameTime }
                            entity.SetHunterState state world
                    else CharacterDispatcher.processEnemyNavigation Hunter.WalkSpeed Hunter.TurnSpeed wayPointPosition entity world
                | None -> ()
            | Some _ | None ->
                entity.LinearVelocity.Map ((*) 0.5f) world
                entity.AngularVelocity.Map ((*) 0.5f) world

    static let processHunterState targetPosition targetBodyIds targetActionState (entity : Entity) (world : World) =

        // process target sighting
        let position = entity.GetPosition world
        let rotation = entity.GetRotation world
        let bodyId = entity.GetBodyId world
        if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId targetBodyIds world then
            let state = entity.GetHunterState world
            let state =
                match targetActionState with
                | HideState hide ->
                    match hide.HidePhase with
                    | HideEntering -> { state with HunterAwareness = AwareOfTargetHiding world.GameTime }
                    | HideWaiting -> state
                    | HideEmerging -> { state with HunterAwareness = AwareOfTargetTraversing world.GameTime }
                    | HideUncovered -> state
                | _ -> { state with HunterAwareness = AwareOfTargetTraversing world.GameTime }
            entity.SetHunterState state world

        // process hunter state
        let uncoveredPlayer =
            let state = entity.GetHunterState world
            match state.HunterAwareness with
            | UnawareOfTarget ->
                processHunterWayPointNavigation entity world
                false
            | AwareOfTargetTraversing startTime ->
                if GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetTraversingDuration = 1.0f then
                    entity.SetHunterState { state with HunterAwareness = UnawareOfTarget } world
                    false
                else
                    CharacterDispatcher.processEnemyAggression Hunter.WalkSpeed Hunter.TurnSpeed targetPosition targetBodyIds entity world
                    false
            | AwareOfTargetHiding startTime ->
                if GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetHidingDuration = 1.0f then
                    entity.SetHunterState { state with HunterAwareness = UnawareOfTarget } world
                    false
                elif CharacterDispatcher.processEnemyUncovering Hunter.WalkSpeed Hunter.TurnSpeed targetPosition entity world then
                    entity.SetHunterState { state with HunterAwareness = AwareOfTargetTraversing world.GameTime } world
                    true
                else false

        // fin
        uncoveredPlayer

    static member Properties =
        let characterType = Hunter
        [define Entity.Persistent characterType.Persistent
         define Entity.BodyShape characterType.BodyShape
         define Entity.CharacterProperties characterType.CharacterProperties
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterType characterType
         define Entity.HunterState HunterState.initial]

    override this.ProcessCharacterState (entity, world) =
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
            match enemyTargetingEir with
            | Right (targetPosition, targetBodyIds, targetActionState) ->
                let uncoveredPlayer = processHunterState targetPosition targetBodyIds targetActionState entity world
                if uncoveredPlayer && player.GetExists world then
                    player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideUncovered }) world
            | Left () -> ()

    override this.ProcessCharacterView (entity, world) =
        let positionInterpolated = entity.GetPositionInterpolated world
        let rotationInterpolated = entity.GetRotationInterpolated world
        World.doAnimatedModel Constants.Gameplay.CharacterAnimatedModelName
            [Entity.Position @= positionInterpolated
             Entity.Rotation @= rotationInterpolated
             Entity.Size .= entity.GetSize world
             Entity.Offset .= entity.GetOffset world
             Entity.MountOpt .= None
             Entity.Pickable .= false
             Entity.AnimatedModel .= Assets.Gameplay.CruciformModel
             Entity.MaterialProperties .= { MaterialProperties.defaultProperties with ScatterTypeOpt = ValueSome SkinScatter }] world
        world.DeclaredEntity

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            for sightRay in Algorithm.computeSightRays Constants.Gameplay.EnemySightDistance position rotation do
                let segment = Segment3 (sightRay.Origin, sightRay.Origin + sightRay.Direction)
                World.imGuiSegment3d segment 1.0f Color.Red world
        | _ -> ()