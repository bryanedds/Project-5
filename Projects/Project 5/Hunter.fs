namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module HunterDispatcherExtensions =
    type Entity with
        member this.GetWayPoints world : WayPoint array = this.Get (nameof this.WayPoints) world
        member this.SetWayPoints (value : WayPoint array) world = this.Set (nameof this.WayPoints) value world
        member this.WayPoints = lens (nameof this.WayPoints) this this.GetWayPoints this.SetWayPoints
        member this.GetWayPointPlayback world : Playback = this.Get (nameof this.WayPointPlayback) world
        member this.SetWayPointPlayback (value : Playback) world = this.Set (nameof this.WayPointPlayback) value world
        member this.WayPointPlayback = lens (nameof this.WayPointPlayback) this this.GetWayPointPlayback this.SetWayPointPlayback
        member this.GetWayPointBouncing world : bool = this.Get (nameof this.WayPointBouncing) world
        member this.SetWayPointBouncing (value : bool) world = this.Set (nameof this.WayPointBouncing) value world
        member this.WayPointBouncing = lens (nameof this.WayPointBouncing) this this.GetWayPointBouncing this.SetWayPointBouncing
        member this.GetWayPointIndexOpt world : int option = this.Get (nameof this.WayPointIndexOpt) world
        member this.SetWayPointIndexOpt (value : int option) world = this.Set (nameof this.WayPointIndexOpt) value world
        member this.WayPointIndexOpt = lens (nameof this.WayPointIndexOpt) this this.GetWayPointIndexOpt this.SetWayPointIndexOpt
        member this.GetWayPointTimeOpt world : GameTime option = this.Get (nameof this.WayPointTimeOpt) world
        member this.SetWayPointTimeOpt (value : GameTime option) world = this.Set (nameof this.WayPointTimeOpt) value world
        member this.WayPointTimeOpt = lens (nameof this.WayPointTimeOpt) this this.GetWayPointTimeOpt this.SetWayPointTimeOpt
        member this.GetAwareness world : Awareness = this.Get (nameof this.Awareness) world
        member this.SetAwareness (value : Awareness) world = this.Set (nameof this.Awareness) value world
        member this.Awareness = lens (nameof this.Awareness) this this.GetAwareness this.SetAwareness

type HunterDispatcher () =
    inherit CharacterDispatcher ()

    static let processHunterWayPointNavigation (entity : Entity) world =
        match entity.GetWayPoints world with
        | [||] -> ()
        | wayPoints ->
            match entity.GetWayPointIndexOpt world with
            | Some wayPointIndex when wayPointIndex < wayPoints.Length ->
                let wayPoint = wayPoints.[wayPointIndex]
                match tryResolve wayPoint.WayPoint entity with
                | Some wayPointEntity ->
                    let wayPointPosition = wayPointEntity.GetPosition world
                    let wayPointDistance = wayPointPosition.Distance (entity.GetPosition world)
                    if wayPointDistance < Constants.Gameplay.HuntWayPointProximity then
                        match entity.GetWayPointTimeOpt world with
                        | Some wayPointTime ->
                            let waitTime = world.GameTime - wayPointTime
                            if waitTime >= wayPoint.WayPointWaitTime then
                                let (wayPointIndexOpt, wayPointBouncing) =
                                    match entity.GetWayPointPlayback world with
                                    | Once ->
                                        let wayPointIndex = inc wayPointIndex
                                        if wayPointIndex < wayPoints.Length
                                        then (Some wayPointIndex, false)
                                        else (None, false)
                                    | Loop ->
                                        let wayPointIndex = inc wayPointIndex % wayPoints.Length
                                        (Some wayPointIndex, false)
                                    | Bounce ->
                                        if entity.GetWayPointBouncing world then
                                            let wayPointIndex = dec wayPointIndex
                                            if wayPointIndex < 0
                                            then (Some (inc wayPointIndex), false)
                                            else (Some wayPointIndex, true)
                                        else
                                            let wayPointIndex = inc wayPointIndex
                                            if wayPointIndex = wayPoints.Length
                                            then (Some (dec wayPointIndex), true)
                                            else (Some wayPointIndex, false)
                                entity.SetWayPointBouncing wayPointBouncing world
                                entity.SetWayPointIndexOpt wayPointIndexOpt world
                                entity.SetWayPointTimeOpt None world
                            else
                                entity.LinearVelocity.Map ((*) 0.5f) world
                                entity.AngularVelocity.Map ((*) 0.5f) world
                        | None -> entity.SetWayPointTimeOpt (Some world.GameTime) world
                    else CharacterDispatcher.processEnemyNavigation wayPointPosition entity world
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
            match targetActionState with
            | HideState hide ->
                match hide.HidePhase with
                | HideEntering -> entity.SetAwareness (AwareOfTargetHiding world.GameTime) world
                | HideWaiting -> ()
                | HideEmerging -> entity.SetAwareness (AwareOfTargetTraversing world.GameTime) world
                | HideUncovered -> ()
            | _ -> entity.SetAwareness (AwareOfTargetTraversing world.GameTime) world

        // process hunter state
        let uncoveredPlayer =
            match entity.GetAwareness world with
            | UnawareOfTarget ->
                processHunterWayPointNavigation entity world
                false
            | AwareOfTargetTraversing startTime ->
                if GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetTraversingDuration = 1.0 then
                    entity.SetAwareness UnawareOfTarget world
                    false
                else
                    CharacterDispatcher.processEnemyAggression targetPosition targetBodyIds entity world
                    false
            | AwareOfTargetHiding startTime ->
                if GameTime.progress startTime world.GameTime Constants.Gameplay.AwareOfTargetHidingDuration = 1.0 then
                    entity.SetAwareness UnawareOfTarget world
                    false
                elif CharacterDispatcher.processEnemyUncovering targetPosition entity world then
                    entity.SetAwareness (AwareOfTargetTraversing world.GameTime) world
                    true
                else false

        // fin
        uncoveredPlayer

    static member Properties =
        let characterType = Hunter
        [define Entity.BodyShape characterType.BodyShape
         define Entity.CharacterProperties characterType.CharacterProperties
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterType characterType
         define Entity.WayPoints [||]
         define Entity.WayPointPlayback Loop
         define Entity.WayPointBouncing false
         define Entity.WayPointIndexOpt None
         define Entity.WayPointTimeOpt None
         define Entity.Awareness UnawareOfTarget]

    override this.ProcessCharacterState (entity, world) =
        if world.Advancing then
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
                if uncoveredPlayer then player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideUncovered }) world
            | Left () -> ()

    override this.DeclareCharacterView (entity, world) =
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