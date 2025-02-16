namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type WayPoint =
    { WayPoint : Entity Relation
      WayPointWaitTime : single }

type [<SymbolicExpansion>] HunterState =
    { HunterWayPoints : WayPoint array
      HunterWayPointPlayback : Playback
      HunterWayPointIndexOpt : int option
      HunterWayPointCountDownOpt : single option
      HunterAwareOfPlayerOpt : single option }

    static member initial =
        { HunterWayPoints = [||]
          HunterWayPointPlayback = Loop
          HunterWayPointIndexOpt = None
          HunterWayPointCountDownOpt = None
          HunterAwareOfPlayerOpt = None }

type StalkerSpawn =
    | StalkerUnspawned of CountDown : single
    | StalkerSpawned of SpawnPoint : Entity Relation * CountDown : single
    | StalkerUnspawning of UnspawnPoint : Entity Relation

    static member update deltaTime spawnAllowed spawnPoints spawn =
        if spawnAllowed then
            match spawn with
            | StalkerUnspawned countDown ->
                let countDown = countDown - deltaTime
                if countDown <= 0.0f
                then (true, StalkerSpawned (Gen.randomItem spawnPoints, 90.0f + Gen.randomf1 90.0f))
                else (false, StalkerUnspawned countDown)
            | StalkerSpawned (spawnPoint, countDown) ->
                let countDown = countDown - deltaTime
                if countDown <= 0.0f
                then (true, StalkerUnspawning spawnPoint)
                else (false, StalkerSpawned (spawnPoint, countDown))
            | StalkerUnspawning _ ->
                (false, spawn)
        else
            match spawn with
            | StalkerSpawned (spawnPoint, _) -> (true, StalkerUnspawning spawnPoint)
            | StalkerUnspawned _ | StalkerUnspawning _ -> (false, spawn)

    static member initial =
        StalkerUnspawned (120.0f + Gen.randomf1 120.0f)

type [<SymbolicExpansion>] StalkerState =
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

type [<SymbolicExpansion>] PlayerState =
    { HideStateOpt : HideState option }

    static member initial =
        { HideStateOpt = None  }

type CharacterState =
    | HunterState of HunterState
    | StalkerState of StalkerState
    | PlayerState of PlayerState

    member this.CharacterType =
        match this with
        | HunterState _ -> Hunter
        | StalkerState _ -> Stalker
        | PlayerState _ -> Player

and CharacterType =
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

    member this.CharacterProperties =
        match this with
        | Hunter | Stalker -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }
        | Player -> CharacterProperties.defaultProperties

    member this.InitialState =
        match this with
        | Hunter -> HunterState HunterState.initial
        | Stalker -> StalkerState StalkerState.initial
        | Player -> PlayerState PlayerState.initial

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