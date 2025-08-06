namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
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

type [<AbstractClass>] CharacterDispatcher () =
    inherit Entity3dDispatcherImSim (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpoledFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.Static false
         define Entity.BodyType KinematicCharacter
         define Entity.Substance (Mass 50.0f)
         define Entity.ActionState NormalState
         define Entity.MovementState (Standing 0.0f)
         define Entity.HitPoints 1
         define Entity.InsertionSpotCollisions Set.empty
         define Entity.DoorSpotCollisions Set.empty
         define Entity.InvestigationSpotCollisions Set.empty
         define Entity.HidingSpotCollisions Set.empty
         define Entity.WeaponCollisions Set.empty
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    static member processEnemyNavigation (goalPosition : Vector3) (entity : Entity) world =
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
            entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            entity.SetAngularVelocity followOutput.NavAngularVelocity world
            entity.SetRotation followOutput.NavRotation world
        | None -> ()

    static member processEnemyAggression (targetPosition : Vector3) (targetBodyIds : BodyId Set) (entity : Entity) world =

        // attacking
        match entity.GetActionState world with
        | NormalState ->
            let position = entity.GetPosition world
            let positionFlat = position.WithY 0.0f
            let rotation = entity.GetRotation world
            let bodyId = entity.GetBodyId world
            if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId targetBodyIds world then
                let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
                let playerPositionFlat = targetPosition.WithY 0.0f
                if  playerPositionFlat.Distance positionFlat < 1.0f &&
                    rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                    entity.SetActionState (AttackState (AttackState.make world.GameTime)) world
                    entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
        | _ -> ()

        // navigation
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
            entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            entity.SetAngularVelocity followOutput.NavAngularVelocity world
            entity.SetRotation followOutput.NavRotation world
        | None -> ()

    static member processEnemyUncovering (targetPosition : Vector3) (entity : Entity) world =

        // opening door
        let uncoveredPlayer =
            match entity.GetActionState world with
            | NormalState ->
                let doorSpotCollisions = entity.GetDoorSpotCollisions world
                match Seq.tryHead doorSpotCollisions with
                | Some doorSpot ->
                    match doorSpot.GetDoorState world with
                    | DoorClosed ->
                        doorSpot.SetDoorState (DoorOpening world.GameTime) world
                        true
                    | _ -> false
                | None -> false
            | _ -> false

        // navigation
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
            entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            entity.SetAngularVelocity followOutput.NavAngularVelocity world
            entity.SetRotation followOutput.NavRotation world
        | None -> ()

        // fin
        uncoveredPlayer

    override this.Process (entity, world) =

        // process body events
        let bodyEvents = World.doSubscriptionToBodyEvents "BodyEvents" entity world
        for bodyEvent in bodyEvents do
            match bodyEvent with
            | BodyPenetrationData penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee ->
                    if penetratee.Is<InsertionSpotDispatcher> world then
                        entity.InsertionSpotCollisions.Map (Set.add penetratee) world
                    elif penetratee.Is<DoorSpotDispatcher> world then
                        entity.DoorSpotCollisions.Map (Set.add penetratee) world
                    elif penetratee.Is<InvestigationSpotDispatcher> world then
                        entity.InvestigationSpotCollisions.Map (Set.add penetratee) world
                    elif penetratee.Is<HidingSpotDispatcher> world then
                        entity.HidingSpotCollisions.Map (Set.add penetratee) world
                | _ -> ()
            | BodySeparationExplicitData explicit ->
                match explicit.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee ->
                    entity.InsertionSpotCollisions.Map (Set.remove separatee) world
                    entity.DoorSpotCollisions.Map (Set.remove separatee) world
                    entity.InvestigationSpotCollisions.Map (Set.remove separatee) world
                    entity.HidingSpotCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodySeparationImplicitData implicit ->
                match implicit.BodyId.BodySource with
                | :? Entity as separatee ->
                    entity.InsertionSpotCollisions.Map (Set.remove separatee) world
                    entity.DoorSpotCollisions.Map (Set.remove separatee) world
                    entity.InvestigationSpotCollisions.Map (Set.remove separatee) world
                    entity.HidingSpotCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodyTransformData _ -> ()

        // unmount when advancing to enable physics
        if world.Advancing
        then entity.SetMountOptWithAdjustment None world
        else entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world

        // process expanded hide sensor on state
        let characterType = entity.GetCharacterType world
        let expandedHideSensorBodyEnabled =
            match entity.GetActionState world with
            | HideState hide -> hide.HidePhase.IsHideEntering
            | _ -> false
        let (_, _) =
            World.doSensorModel Constants.Gameplay.CharacterExpandedHideSensorName
                [Entity.PositionLocal @= entity.GetPosition world
                 Entity.VisibleLocal .= false
                 Entity.BodyEnabled @= expandedHideSensorBodyEnabled
                 Entity.BodyShape .= characterType.ExpandedHideSensorBodyShape
                 Entity.MountOpt .= None] world

        // process character state
        this.ProcessCharacterState (entity, world)

        // process action state
        if world.Advancing then
            match entity.GetActionState world with
            | NormalState -> ()
            | AttackState attack as actionState ->
                let localTime = world.GameTime - attack.AttackTime
                let actionState = if localTime <= 0.92f then actionState else NormalState
                entity.SetActionState actionState world
            | InventoryState -> ()
            | InsertionState _ -> ()
            | InvestigationState _ -> ()
            | HideState hide ->
                match hide.HidePhase with
                | HideEntering ->
                    let localTime = world.GameTime - hide.HideTime
                    if localTime >= 1.5f then
                        entity.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideWaiting }) world
                | HideWaiting -> ()
                | HideEmerging ->
                    let localTime = world.GameTime - hide.HideTime
                    if localTime >= 1.5f then entity.SetActionState NormalState world
                | HideUncovered -> ()
            | InjuryState injury as actionState ->
                let localTime = world.GameTime - injury.InjuryTime
                let injuryTime = characterType.InjuryTime
                let actionState = if localTime < injuryTime then actionState else NormalState
                entity.SetActionState actionState world
            | WoundState _ -> ()

        // declare character view
        let animatedModel = this.DeclareCharacterView (entity, world)

        // process traversal animations
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
            if List.hasAtLeast 2 animations then
                if movementState.IsStanding then
                    entity.SetMovementState (Walking (world.GameTime, world.GameTime)) world
            else
                if movementState.IsWalking then
                    entity.SetMovementState (Standing world.GameTime) world
            animatedModel.SetAnimations (Array.ofList animations) world
        | _ -> ()

        // process action animations
        match entity.GetActionState world with
        | NormalState ->
            match entity.GetMovementState world with
            | Standing _ -> ()
            | Walking (startTime, lastStepTime) ->
                let strideTime = GameTime.ofSeconds 0.75f
                let offsetTime = GameTime.ofSeconds 0.255f
                let localStepTime = world.GameTime - lastStepTime + offsetTime
                if localStepTime >= strideTime then
                    World.playSound 0.25f Assets.Gameplay.StepSound world
                    entity.SetMovementState (Walking (startTime, lastStepTime + strideTime)) world
        | AttackState attack ->
            let localTime = world.GameTime - attack.AttackTime
            if localTime > 0.12f && not attack.AttackSoundPlayed then
                let attack = { attack with AttackSoundPlayed = true }
                entity.SetActionState (AttackState attack) world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
            let animation = Animation.once attack.AttackTime None "Attack"
            animatedModel.SetAnimations [|animation|] world
        | InventoryState -> ()
        | InsertionState insertion -> ()
        | InvestigationState investigation -> ()
        | HideState hide -> ()
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
        let (_, results) =
            World.doRigidModel Constants.Gameplay.CharacterWeaponName
                [Entity.Position @= weaponTransform.Translation
                 Entity.Rotation @= weaponTransform.Rotation
                 Entity.Offset .= v3 0.0f 0.5f 0.0f
                 Entity.MountOpt .= None
                 Entity.Visible .= false
                 Entity.Pickable .= false
                 Entity.StaticModel @= entity.GetWeaponModel world
                 Entity.BodyEnabled @= characterType.IsEnemy
                 Entity.BodyType .= Static
                 Entity.BodyShape .= BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor .= true
                 Entity.NavShape .= EmptyNavShape] world

        // process weapon collisions
        for result in results do
            match result with
            | BodyPenetrationData penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                    if characterType.IsPlayer <> (penetratee.GetCharacterType world).IsPlayer then
                        entity.WeaponCollisions.Map (Set.add penetratee) world
                | _ -> ()
            | BodySeparationExplicitData explicit ->
                match explicit.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodySeparationImplicitData implicit ->
                match implicit.BodyId.BodySource with
                | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                | _ -> ()
            | BodyTransformData _ -> ()

        // process attacks
        match entity.GetActionState world with
        | AttackState attack ->
            let localTime = world.GameTime - attack.AttackTime
            if localTime >= 0.2f && localTime < 0.8f then
                let weaponCollisions = entity.GetWeaponCollisions world
                let attackedCharacters = Set.difference weaponCollisions attack.AttackedCharacters
                entity.SetActionState (AttackState { attack with AttackedCharacters = Set.union attack.AttackedCharacters weaponCollisions }) world
                for character in attackedCharacters do
                    World.publish character entity.AttackEvent entity world
            else entity.SetActionState (AttackState attack) world
        | _ -> ()

        // process death
        match entity.GetActionState world with
        | WoundState wound when wound.WoundTime >= world.GameTime - GameTime.ofSeconds 1.0f && not wound.WoundEventPublished ->
            World.publish entity entity.DeathEvent entity world
            let wound = { wound with WoundEventPublished = true }
            entity.SetActionState (WoundState wound) world
        | _ -> ()

    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

    /// Process the character state.
    abstract ProcessCharacterState : Entity * World -> unit

    /// Process the character view, returning its animated model.
    abstract DeclareCharacterView : Entity * World -> Entity