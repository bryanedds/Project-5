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

type CharacterType =
    | Hunter
    | Stalker
    | Player

    member this.IsEnemy =
        not this.IsPlayer

    member this.Persistent =
        not this.IsPlayer

    member this.HitPointsMax =
        match this with
        | Hunter -> 1
        | Stalker -> Int32.MaxValue
        | Player -> 3

    member this.WalkSpeed =
        match this with
        | Hunter -> 1.375f
        | Stalker -> 1.0f
        | Player -> 1.75f

    member this.TurnSpeed =
        match this with
        | Hunter -> 2.5f
        | Stalker -> 2.0f
        | Player -> 1.0f

    member this.InjuryTime =
        match this with
        | Hunter -> 0.667f
        | Stalker -> 0.25f
        | Player -> 0.5f

    member this.AnimatedModel =
        match this with
        | Hunter -> Assets.Gameplay.RhyoliteModel
        | Stalker -> Assets.Gameplay.CruciformModel
        | Player -> Assets.Gameplay.SophieModel

    member this.InitialState =
        match this with
        | Hunter -> HunterState HunterState.initial
        | Stalker -> StalkerState StalkerState.initial
        | Player -> PlayerState PlayerState.initial

    member this.CharacterProperties =
        match this with
        | Hunter | Stalker -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }
        | Player -> CharacterProperties.defaultProperties

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