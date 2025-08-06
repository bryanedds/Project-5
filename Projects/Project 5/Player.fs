namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module PlayerDispatcherExtensions =
    type Entity with
        member this.GetPlayerState world : PlayerState = this.Get (nameof this.PlayerState) world
        member this.SetPlayerState (value : PlayerState) world = this.Set (nameof this.PlayerState) value world
        member this.PlayerState = lens (nameof this.PlayerState) this this.GetPlayerState this.SetPlayerState

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static let processPlayerInput (entity : Entity) world =

        // attacking
        if World.isKeyboardKeyPressed KeyboardKey.Enter world && false then
            match entity.GetActionState world with
            | NormalState ->
                entity.SetActionState (AttackState (AttackState.make world.GameTime)) world
                entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
            | _ -> ()

        // movement
        match entity.GetActionState world with
        | NormalState ->
            let rotation = entity.GetRotation world
            let walkSpeed = Player.WalkSpeed
            let forward = rotation.Forward
            let right = rotation.Right
            let walkDirection =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right else v3Zero)
            let walkVelocity = if walkDirection <> v3Zero then walkDirection.Normalized * walkSpeed else v3Zero
            entity.SetLinearVelocity (walkVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
        | _ -> ()

        // rotation
        match entity.GetActionState world with
        | NormalState | InventoryState | InsertionState _  | InvestigationState _ | HideState _ ->
            let rotation = entity.GetRotation world
            let turnSpeed = Player.TurnSpeed
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity * world.GameDelta.Seconds) else rotation
            entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
            entity.SetRotation rotation world
        | AttackState _ | InjuryState _ | WoundState _ -> ()

        // process view flip
        entity.PlayerState.Map (fun state -> { state with ViewFlip = World.isKeyboardShiftDown world }) world

        // toggle flash light
        if World.isKeyboardKeyPressed KeyboardKey.Space world then
            entity.PlayerState.Map (fun state -> { state with FlashLightEnabled = not state.FlashLightEnabled }) world

    static member Properties =
        let characterType = Player
        [define Entity.Persistent characterType.Persistent
         define Entity.BodyShape characterType.BodyShape
         define Entity.CharacterProperties characterType.CharacterProperties
         define Entity.HitPoints characterType.HitPointsMax
         define Entity.CharacterType characterType
         define Entity.PlayerState PlayerState.initial]

    override this.ProcessCharacterState (entity, world) =
        if world.Advancing then
            processPlayerInput entity world

    override this.DeclareCharacterView (entity, world) =

        // declare animated model
        let positionInterpolated = entity.GetPositionInterpolated world
        let rotationInterpolated = entity.GetRotationInterpolated world
        let playerState = entity.GetPlayerState world
        let actionState = entity.GetActionState world
        let eyeDistanceRotation =
            rotationInterpolated *
            Quaternion.CreateFromAxisAngle (v3Up, Constants.Gameplay.PlayerEyeShiftAngle * if playerState.ViewFlip then -1.0f else 1.0f) *
            Quaternion.CreateFromAxisAngle (v3Right, -0.25f)
        let visibilityScalar = Algorithm.computePlayerVisibilityScalar positionInterpolated eyeDistanceRotation actionState entity world
        World.doAnimatedModel Constants.Gameplay.CharacterAnimatedModelName
            [Entity.Position @= positionInterpolated
             Entity.Rotation @= rotationInterpolated
             Entity.Size .= entity.GetSize world
             Entity.Offset .= entity.GetOffset world
             Entity.MountOpt .= None
             Entity.Pickable .= false
             Entity.AnimatedModel .= Assets.Gameplay.AvaModel
             Entity.MaterialProperties @= { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome (colorOne.WithA visibilityScalar); ScatterTypeOpt = ValueSome SkinScatter }
             Entity.VisibleLocal @= (visibilityScalar > 0.0f)
             Entity.RenderStyle @= if visibilityScalar = 1.0f then Deferred else Forward (0.0f, 0.0f)
             Entity.DualRenderedSurfaceIndices @= if visibilityScalar = 1.0f then Set.singleton 3 else Set.empty
             Entity.SubsortOffsets .= Map.ofList [(3, -1.0f); (8, -2.0f); (14, -1.0f)]] world
        let animatedModel = world.DeclaredEntity

        // declare flash light
        let positionInterpolated = entity.GetPositionInterpolated world
        let rotationInterpolated = entity.GetRotationInterpolated world
        let state = entity.GetPlayerState world
        World.doLight3d Constants.Gameplay.CharacterLightName
            [Entity.Position @= positionInterpolated + v3Up * 1.2f + rotationInterpolated.Forward * 0.25f
             Entity.Rotation @= rotationInterpolated * Quaternion.CreateFromAxisAngle (v3Right, MathF.PI_OVER_2)
             Entity.MountOpt .= None
             Entity.Static .= false
             Entity.LightType .= SpotLight (0.8f, 1.2f)
             Entity.LightCutoff .= 11.0f
             Entity.Brightness .= 0.8f
             Entity.DesireShadows .= true
             Entity.VisibleLocal @= state.FlashLightEnabled] world

        // declare player hearts
        let hitPoints = entity.GetHitPoints world
        for i in 0 .. dec Player.HitPointsMax do
            World.doStaticSprite ("Heart+" + string i)
                [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                 Entity.Size .= v3 32.0f 32.0f 0.0f
                 Entity.MountOpt .= None
                 Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty] world
    
        // fin
        animatedModel