namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type WayPoint =
    { WayPoint : Entity Relation
      WayPointWaitTime : GameTime }

type HunterState =
    { HunterWayPoints : WayPoint list
      HunterWayPointIndexOpt : int option
      HunterWayPointCountDownOpt : GameTime option
      HunterAwareOfPlayerOpt : GameTime option }

    static member initial =
        { HunterWayPoints = []
          HunterWayPointIndexOpt = None
          HunterWayPointCountDownOpt = None
          HunterAwareOfPlayerOpt = None }

type StalkerSpawn =
    | StalkerUnspawned of CountDown : GameTime
    | StalkerSpawned of SpawnPoint : Entity Relation * CountDown : GameTime
    | StalkerUnspawning of UnspawnPoint : Entity Relation

    static member update deltaTime spawnAllowed spawnPoints spawn =
        if spawnAllowed then
            match spawn with
            | StalkerUnspawned countDown ->
                let countDown = countDown - deltaTime
                if countDown <= GameTime.zero
                then (true, StalkerSpawned (Gen.randomItem spawnPoints, GameTime.ofSeconds (90.0f + Gen.randomf1 90.0f)))
                else (false, StalkerUnspawned countDown)
            | StalkerSpawned (spawnPoint, countDown) ->
                let countDown = countDown - deltaTime
                if countDown <= GameTime.zero
                then (true, StalkerUnspawning spawnPoint)
                else (false, StalkerSpawned (spawnPoint, countDown))
            | StalkerUnspawning _ ->
                (false, spawn)
        else
            match spawn with
            | StalkerSpawned (spawnPoint, _) -> (true, StalkerUnspawning spawnPoint)
            | StalkerUnspawned _ | StalkerUnspawning _ -> (false, spawn)

    static member initial =
        StalkerUnspawned (GameTime.ofSeconds (120.0f + Gen.randomf1 120.0f))

type StalkerState =
    { StalkerSpawn : StalkerSpawn
      StalkerSpawnAllowed : bool
      StalkerSpawnPoints : Entity Relation list }

    static member initial =
        { StalkerSpawn = StalkerSpawn.initial
          StalkerSpawnAllowed = true
          StalkerSpawnPoints = [] }

type HideType =
    | HideStanding // like a locker or tall cupboard
    | HideKneeling // like in a floor cabinet
    | HideLying // like under a bed

type HidePhase =
    | HideEntering
    | HideWaiting
    | HideExiting

type HideState =
    { HideTime : GameTime
      HidePhase : HidePhase }

type PlayerState =
    { HideStateOpt : HideState option }

    static member initial =
        { HideStateOpt = None  }

type CharacterState =
    | HunterState of HunterState
    | StalkerState of StalkerState
    | PlayerState of PlayerState

    member this.Persistent =
        not this.IsPlayerState

    member this.IsEnemyState =
        not this.IsPlayerState

    member this.HitPointsMax =
        match this with
        | HunterState _ -> 1
        | StalkerState _ -> Int32.MaxValue
        | PlayerState _ -> 3

    member this.WalkSpeed =
        match this with
        | HunterState _ -> 1.375f
        | StalkerState _ -> 1.0f
        | PlayerState _ -> 1.75f

    member this.TurnSpeed =
        match this with
        | HunterState _ -> 2.5f
        | StalkerState _ -> 2.0f
        | PlayerState _ -> 1.0f

    member this.AnimatedModel =
        match this with
        | HunterState _ -> Assets.Gameplay.RhyoliteModel
        | StalkerState _ -> Assets.Gameplay.CruciformModel
        | PlayerState _ -> Assets.Gameplay.SophieModel

type AttackState =
    { AttackTime : single
      AttackSoundPlayed : bool
      AttackedCharacters : Entity Set }

    static member make time =
        { AttackTime = time
          AttackSoundPlayed = false
          AttackedCharacters = Set.empty }

type InjuryState =
    { InjuryTime : single }

type WoundState =
    { WoundTime : single
      WoundEventPublished : bool }

type ActionState =
    | NormalState
    | AttackState of AttackState
    | InjuryState of InjuryState
    | WoundState of WoundState

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacterState world : CharacterState = this.Get (nameof this.CharacterState) world
        member this.SetCharacterState (value : CharacterState) world = this.Set (nameof this.CharacterState) value world
        member this.CharacterState = lens (nameof this.CharacterState) this this.GetCharacterState this.SetCharacterState
        member this.GetActionState world : ActionState = this.Get (nameof this.ActionState) world
        member this.SetActionState (value : ActionState) world = this.Set (nameof this.ActionState) value world
        member this.ActionState = lens (nameof this.ActionState) this this.GetActionState this.SetActionState
        member this.GetHitPoints world : int = this.Get (nameof this.HitPoints) world
        member this.SetHitPoints (value : int) world = this.Set (nameof this.HitPoints) value world
        member this.HitPoints = lens (nameof this.HitPoints) this this.GetHitPoints this.SetHitPoints
        member this.GetCharacterCollisions world : Entity Set = this.Get (nameof this.CharacterCollisions) world
        member this.SetCharacterCollisions (value : Entity Set) world = this.Set (nameof this.CharacterCollisions) value world
        member this.CharacterCollisions = lens (nameof this.CharacterCollisions) this this.GetCharacterCollisions this.SetCharacterCollisions
        member this.GetWeaponCollisions world : Entity Set = this.Get (nameof this.WeaponCollisions) world
        member this.SetWeaponCollisions (value : Entity Set) world = this.Set (nameof this.WeaponCollisions) value world
        member this.WeaponCollisions = lens (nameof this.WeaponCollisions) this this.GetWeaponCollisions this.SetWeaponCollisions
        member this.GetWeaponModel world : StaticModel AssetTag = this.Get (nameof this.WeaponModel) world
        member this.SetWeaponModel (value : StaticModel AssetTag) world = this.Set (nameof this.WeaponModel) value world
        member this.WeaponModel = lens (nameof this.WeaponModel) this this.GetWeaponModel this.SetWeaponModel
        member this.AttackEvent = Events.AttackEvent --> this
        member this.DieEvent = Events.DieEvent --> this

        member this.GetIsEnemy world = (this.GetCharacterState world).IsEnemyState
        member this.GetIsPlayer world = (this.GetCharacterState world).IsPlayerState
        member this.GetHitPointsMax world = (this.GetCharacterState world).HitPointsMax
        member this.GetWalkSpeed world = (this.GetCharacterState world).WalkSpeed
        member this.GetTurnSpeed world = (this.GetCharacterState world).TurnSpeed
        member this.GetAnimatedModel' world = (this.GetCharacterState world).AnimatedModel

        member this.GetCharacterProperties world =
            if this.GetIsEnemy world
            then { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }
            else CharacterProperties.defaultProperties

type CharacterDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static let processEnemyInput (playerPosition : Vector3) (entity : Entity) world =

        // attacking
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let position = entity.GetPosition world
                let positionFlat = position.WithY 0.0f
                let rotation = entity.GetRotation world
                let rotationForwardFlat = rotation.Forward.WithY(0.0f).Normalized
                let playerPositionFlat = playerPosition.WithY 0.0f
                if position.Y - playerPosition.Y >= 0.25f then // above player
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 0.75f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.1f then
                        let world = entity.SetActionState (AttackState (AttackState.make world.ClockTime)) world
                        entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                    else world
                elif playerPosition.Y - position.Y < 1.3f then // at or a bit below player
                    if  Vector3.Distance (playerPositionFlat, positionFlat) < 1.0f &&
                        rotationForwardFlat.AngleBetween (playerPositionFlat - positionFlat) < 0.15f then
                        let world = entity.SetActionState (AttackState (AttackState.make world.ClockTime)) world
                        entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                    else world
                else world
            | _ -> world

        // navigation
        let (navSpeedsOpt, world) =
            let actionState = entity.GetActionState world
            match actionState with
            | NormalState ->
                let navSpeed =
                    if actionState = NormalState
                    then (entity.GetWalkSpeed world, entity.GetTurnSpeed world)
                    else (0.0f, entity.GetTurnSpeed world * 3.0f)
                let world = entity.SetActionState actionState world
                (Some navSpeed, world)
            | _ -> (None, world)
        match navSpeedsOpt with
        | Some (moveSpeed, turnSpeed) ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let sphere =
                if position.Y - playerPosition.Y >= 0.25f
                then Sphere (playerPosition, 0.1f) // when above player
                else Sphere (playerPosition, 0.4f) // when at or below player
            let nearest = sphere.Nearest position
            let followOutput = World.nav3dFollow (Some 0.5f) (Some 12.0f) moveSpeed turnSpeed position rotation nearest Simulants.Gameplay world    
            let world = entity.SetLinearVelocity (followOutput.NavLinearVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
            let world = entity.SetRotation followOutput.NavRotation world
            world
        | None -> world

    static let processPlayerInput (entity : Entity) world =

        // action
        let world =

            // attacking
            if World.isKeyboardKeyPressed KeyboardKey.RShift world then
                match entity.GetActionState world with
                | NormalState ->
                    let world = entity.SetActionState (AttackState (AttackState.make world.ClockTime)) world
                    entity.SetLinearVelocity (v3Up * entity.GetLinearVelocity world) world
                | AttackState _ | InjuryState _ | WoundState _ -> world

            // do nothing
            else world

        // movement
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        if entity.GetActionState world = NormalState || not grounded then

            // compute new position
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let walkSpeed = entity.GetWalkSpeed world * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)

            // compute new rotation
            let turnSpeed = entity.GetTurnSpeed world * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity * world.GameDelta.Seconds) else rotation

            // apply changes
            let world = entity.SetLinearVelocity (walkVelocity.WithY 0.0f + v3Up * entity.GetLinearVelocity world) world
            let world = entity.SetAngularVelocity (v3 0.0f turnVelocity 0.0f) world
            let world = entity.SetRotation rotation world
            world

        // no movement
        else world

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Size (v3Dup 2.0f)
         define Entity.Offset (v3 0.0f 1.0f 0.0f)
         define Entity.Static false
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })
         define Entity.Substance (Mass 50.0f)
         define Entity.Observable true
         define Entity.CharacterState (HunterState HunterState.initial)
         define Entity.ActionState NormalState
         define Entity.HitPoints 1
         define Entity.CharacterCollisions Set.empty
         define Entity.WeaponCollisions Set.empty
         define Entity.WeaponModel Assets.Gameplay.GreatSwordModel]

    override this.Process (entity, world) =

        // process penetration
        let (penetrations, world) = World.doSubscription "Penetration" entity.BodyPenetrationEvent world
        let world =
            FQueue.fold (fun world penetration ->
                match penetration.BodyShapePenetratee.BodyId.BodySource with
                | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world ->
                    if entity.GetIsEnemy world && penetratee.GetIsEnemy world
                    then entity.CharacterCollisions.Map (Set.add penetratee) world
                    else world
                | _ -> world)
                world penetrations

        // process separation (explicit)
        let (separationsExplicit, world) = World.doSubscription "SeparationExplicit" entity.BodySeparationExplicitEvent world
        let world =
            FQueue.fold (fun world separation ->
                match separation.BodyShapeSeparatee.BodyId.BodySource with
                | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                    entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world separationsExplicit

        // process separation (implicit)
        let (separationsImplicit, world) = World.doSubscription "SeparationImplicit" entity.BodySeparationImplicitEvent world
        let world =
            FQueue.fold (fun world (separation : BodySeparationImplicitData) ->
                match separation.BodyId.BodySource with
                | :? Entity as separatee -> entity.CharacterCollisions.Map (Set.remove separatee) world
                | _ -> world)
                world separationsImplicit

        // process input
        let world =
            if world.Advancing then
                match entity.GetCharacterState world with
                | HunterState _ | StalkerState _ ->
                    if Simulants.GameplayPlayer.GetExists world
                    then processEnemyInput (Simulants.GameplayPlayer.GetPosition world) entity world
                    else world
                | PlayerState _ -> processPlayerInput entity world
            else world

        // process action state
        let world =
            let actionState =
                match entity.GetActionState world with
                | NormalState | WoundState _ as actionState ->
                    actionState
                | AttackState attack as actionState ->
                    let localTime = world.ClockTime - attack.AttackTime
                    if localTime <= 0.92f then actionState else NormalState
                | InjuryState injury as actionState ->
                    let localTime = world.ClockTime - injury.InjuryTime
                    let injuryTime = if entity.GetIsEnemy world then 0.667f else 0.5f
                    if localTime < injuryTime then actionState else NormalState
            entity.SetActionState actionState world

        // declare animated model
        let world =
            World.doEntity<AnimatedModelDispatcher> Constants.Gameplay.CharacterAnimatedModelName
                [Entity.Position @= entity.GetPosition world
                 Entity.Rotation @= entity.GetRotation world
                 Entity.Size .= entity.GetSize world
                 Entity.Offset .= entity.GetOffset world
                 Entity.MountOpt .= None
                 Entity.Pickable .= false
                 Entity.AnimatedModel @= entity.GetAnimatedModel' world]
                world
        let animatedModel = world.RecentEntity

        // process traversal animations
        let world =
            match entity.GetActionState world with
            | NormalState ->
                let rotation = entity.GetRotation world
                let linearVelocity = entity.GetLinearVelocity world
                let angularVelocity = entity.GetAngularVelocity world
                let forwardness = linearVelocity.Dot rotation.Forward
                let backness = linearVelocity.Dot -rotation.Forward
                let rightness = linearVelocity.Dot rotation.Right
                let leftness = linearVelocity.Dot -rotation.Right
                let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 0.5f else 0.0f
                let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 0.5f else 0.0f
                let animations =
                    [Animation.make 0.0f None "Armature|Idle" Loop 1.0f 1.0f None]
                let animations =
                    if forwardness >= 0.01f then Animation.make 0.0f None "Armature|WalkForward" Loop 1.0f forwardness None :: animations
                    elif backness >= 0.01f then Animation.make 0.0f None "Armature|WalkBack" Loop 1.0f backness None :: animations
                    else animations
                let animations =
                    if rightness >= 0.01f then Animation.make 0.0f None "Armature|WalkRight" Loop 1.0f rightness None :: animations
                    elif leftness >= 0.01f then Animation.make 0.0f None "Armature|WalkLeft" Loop 1.0f leftness None :: animations
                    else animations
                let animations =
                    if turnRightness >= 0.01f then Animation.make 0.0f None "Armature|TurnRight" Loop 1.0f turnRightness None :: animations
                    elif turnLeftness >= 0.01f then Animation.make 0.0f None "Armature|TurnLeft" Loop 1.0f turnLeftness None :: animations
                    else animations
                animatedModel.SetAnimations (Array.ofList animations) world
            | _ -> world

        // process action animations
        let world =
            match entity.GetActionState world with
            | NormalState ->
                world
            | AttackState attack ->
                let localTime = world.ClockTime - attack.AttackTime
                let world =
                    if localTime > 0.12f && not attack.AttackSoundPlayed then
                        let attack = { attack with AttackSoundPlayed = true }
                        let world = entity.SetActionState (AttackState attack) world
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.SlashSound world
                        world
                    else world
                let animation = Animation.once attack.AttackTime None "Armature|Attack"
                animatedModel.SetAnimations [|animation|] world
            | InjuryState injury ->
                let animation = Animation.once injury.InjuryTime None "Armature|WalkBack"
                animatedModel.SetAnimations [|animation|] world
            | WoundState wound ->
                let animation = Animation.loop wound.WoundTime None "Armature|WalkBack"
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
                 Entity.Pickable .= false
                 Entity.StaticModel @= entity.GetWeaponModel world
                 Entity.BodyType .= Static
                 Entity.BodyShape .= BoxShape { Size = v3 0.3f 1.2f 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.6f 0.0f)); PropertiesOpt = None }
                 Entity.Sensor .= true
                 Entity.NavShape .= EmptyNavShape]
                world

        // process weapon collisions
        let world =
            FQueue.fold (fun world result ->
                match result with
                | BodyPenetration penetration ->
                    match penetration.BodyShapePenetratee.BodyId.BodySource with
                    | :? Entity as penetratee when penetratee.Is<CharacterDispatcher> world && penetratee <> entity ->
                        if entity.GetIsPlayer world <> penetratee.GetIsPlayer world
                        then entity.WeaponCollisions.Map (Set.add penetratee) world
                        else world
                    | _ -> world
                | BodySeparationExplicit separation ->
                    match separation.BodyShapeSeparatee.BodyId.BodySource with
                    | :? Entity as separatee when separatee.Is<CharacterDispatcher> world && separatee <> entity ->
                        entity.WeaponCollisions.Map (Set.remove separatee) world
                    | _ -> world
                | BodySeparationImplicit separation ->
                    match separation.BodyId.BodySource with
                    | :? Entity as separatee -> entity.WeaponCollisions.Map (Set.remove separatee) world
                    | _ -> world
                | BodyTransform _ -> world)
                world results

        // process attacks
        let (attacks, world) =
            match entity.GetActionState world with
            | AttackState attack ->
                let localTime = world.ClockTime - attack.AttackTime
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
            if entity.GetIsPlayer world then
                let hitPoints = entity.GetHitPoints world
                Seq.fold (fun world i ->
                    World.doStaticSprite ("Heart+" + string i)
                        [Entity.Position .= v3 (-284.0f + single i * 32.0f) -144.0f 0.0f
                         Entity.Size .= v3 32.0f 32.0f 0.0f
                         Entity.MountOpt .= None
                         Entity.StaticImage @= if hitPoints >= inc i then Assets.Gameplay.HeartFull else Assets.Gameplay.HeartEmpty]
                        world)
                    world [0 .. dec (entity.GetHitPointsMax world)]
            else world

        // process death
        let world =
            match entity.GetActionState world with
            | WoundState wound when wound.WoundTime >= world.ClockTime - 1.0f && not wound.WoundEventPublished ->
                let world = World.publish entity entity.DieEvent entity world
                let wound = { wound with WoundEventPublished = true}
                entity.SetActionState (WoundState wound) world
            | _ -> world

        // fin
        world

    // custom definition of ray cast to utilize animated model and weapon
    override this.RayCast (ray, entity, world) =
        let animatedModel = entity / Constants.Gameplay.CharacterAnimatedModelName
        match animatedModel.RayCast ray world with
        | [||] ->
            let weapon = entity / Constants.Gameplay.CharacterWeaponName
            weapon.RayCast ray world
        | intersections -> intersections

type HunterDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterState = HunterState HunterState.initial
        [define Entity.Persistent characterState.Persistent
         define Entity.CharacterState characterState
         define Entity.HitPoints characterState.HitPointsMax]

type StalkerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterState = StalkerState StalkerState.initial
        [define Entity.Persistent characterState.Persistent
         define Entity.CharacterState characterState
         define Entity.HitPoints characterState.HitPointsMax]

type PlayerDispatcher () =
    inherit CharacterDispatcher ()

    static member Properties =
        let characterState = PlayerState PlayerState.initial
        [define Entity.Persistent characterState.Persistent
         define Entity.CharacterState characterState
         define Entity.HitPoints characterState.HitPointsMax]