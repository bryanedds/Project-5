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
      HunterWayPointBouncing : bool
      HunterWayPointIndexOpt : int option
      HunterWayPointTimeOpt : single option
      HunterAwareTimeOpt : single option }

    member this.HunterAwareDurationOpt (time : single) =
        match this.HunterAwareTimeOpt with
        | Some start ->
            let awareTime = time - start
            if awareTime >= Constants.Gameplay.HuntDuration then None else Some awareTime
        | None -> None

    member this.HunterAwareProgressOpt (time : single) =
        match this.HunterAwareDurationOpt time with
        | Some awareTime -> Some (awareTime / Constants.Gameplay.HuntDuration)
        | None -> None

    member this.HunterSightDistance (time : single) =
        match this.HunterAwareDurationOpt time with
        | Some _ -> 7.0f
        | None -> 9.0f

    static member computeScanSegments (time : single) position rotation (state : HunterState) =
        let sightDistance = state.HunterSightDistance time
        let sightPosition = position + v3Up * 1.25f
        let sightRotation = rotation
        seq {
            for i in 0 .. dec 13 do
                let angle = Quaternion.CreateFromAxisAngle (v3Up, single i * 5.0f - 30.0f |> Math.DegreesToRadians)
                let scanRotation = sightRotation * angle
                Segment3 (sightPosition, sightPosition + scanRotation.Forward * sightDistance) }

    static member initial =
        { HunterWayPoints = [||]
          HunterWayPointPlayback = Loop
          HunterWayPointBouncing = false
          HunterWayPointIndexOpt = None
          HunterWayPointTimeOpt = None
          HunterAwareTimeOpt = None }

type [<SymbolicExpansion>] StalkerState =
    | Spawned
    | Unspawning of UnspawnPoint : Vector3

    static member initial = Spawned

    static member computeScanSegments position rotation (_ : StalkerState) =
        let sightDistance = 7.0f
        let sightPosition = position + v3Up * 1.25f
        let sightRotation = rotation
        seq {
            for i in 0 .. dec 13 do
                let angle = Quaternion.CreateFromAxisAngle (v3Up, single i * 5.0f - 30.0f |> Math.DegreesToRadians)
                let scanRotation = sightRotation * angle
                Segment3 (sightPosition, sightPosition + scanRotation.Forward * sightDistance) }

type StalkerSpawnState =
    | StalkerUnspawned of UnspawnTime : single
    | StalkerSpawned of SpawnPoint : Entity * SpawnTime : single
    | StalkerUnspawning of UnspawnPoint : Entity * SpawnTime : single

    member this.SpawnTimeOpt =
        match this with
        | StalkerUnspawned _ -> None
        | StalkerSpawned (_, spawnTime) -> Some spawnTime
        | StalkerUnspawning (_, spawnTime) -> Some spawnTime

    member this.SpawnDurationOpt time =
        match this.SpawnTimeOpt with
        | Some spawnTime -> Some (time - spawnTime)
        | None -> None

    static member initial =
        StalkerUnspawned Single.MaxValue

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
        | Hunter -> 1.25f
        | Stalker -> 1.33f
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
        | Hunter -> Assets.Gameplay.CruciformModel
        | Stalker -> Assets.Gameplay.RhyoliteModel
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