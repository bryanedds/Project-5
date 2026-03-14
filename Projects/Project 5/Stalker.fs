namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module StalkerDispatcherExtensions =
    type Entity with
        member this.GetStalkState world : StalkState = this.Get (nameof this.StalkState) world
        member this.SetStalkState (value : StalkState) world = this.Set (nameof this.StalkState) value world
        member this.StalkState = lens (nameof this.StalkState) this this.GetStalkState this.SetStalkState

type StalkerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterType = Stalker
        [define Entity.BodyShape characterType.BodyShape
         define Entity.CharacterProperties characterType.CharacterProperties
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterType characterType
         define Entity.StalkState StalkState.initial]

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
                    Right (player.GetPosition world, playerBodyIds)
                else Left ()
            match enemyTargetingEir with
            | Right (targetPosition, targetBodyIds) ->
                match entity.GetStalkState world with
                | IdlingState ->
                    ()
                | StalkingState stalking ->
                    if not stalking.Awareness.IsUnawareOfTarget then
                        CharacterDispatcher.processEnemyAggression targetPosition targetBodyIds entity world
                | LeavingState leaving ->
                    CharacterDispatcher.processEnemyNavigation leaving.UnspawnPosition entity world
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
             Entity.AnimatedModel .= Assets.Gameplay.RhyoliteModel
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