namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type WayPoint =
    { WayPoint : Entity Relation
      WayPointWaitTime : GameTime }

type [<SymbolicExpansion>] HunterState =
    { HunterWayPoints : WayPoint array
      HunterWayPointPlayback : Playback
      HunterWayPointBouncing : bool
      HunterWayPointIndexOpt : int option
      HunterWayPointTimeOpt : GameTime option
      HunterAwareTimeOpt : GameTime option }

    member this.HunterAwareDurationOpt (time : GameTime) =
        match this.HunterAwareTimeOpt with
        | Some start ->
            let awareTime = time - start
            if awareTime >= Constants.Gameplay.HuntDuration then None else Some awareTime
        | None -> None

    member this.HunterAwareProgressOpt (time : GameTime) =
        match this.HunterAwareDurationOpt time with
        | Some awareTime -> Some (awareTime / Constants.Gameplay.HuntDuration)
        | None -> None

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

type StalkerSpawnState =
    | StalkerUnspawned of UnspawnTime : GameTime
    | StalkerSpawned of SpawnPoint : Entity * SpawnTime : GameTime
    | StalkerUnspawning of UnspawnPoint : Entity * SpawnTime : GameTime

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

type DoorState =
    | DoorClosed
    | DoorOpening of StartTime : GameTime
    | DoorOpened
    | DoorClosing of StartTime : GameTime

type InvestigationPhase =
    | InvestigationNotStarted
    | InvestigationStarted of GameTime
    | InvestigationFinished of GameTime

type HideType =
    | HideStanding // like a locker or tall cupboard
    | HideKneeling // like in a floor cabinet
    | HideLying // like under a bed

type HidePhase =
    | HideEntering
    | HideWaiting
    | HideExiting

type HidingSpot =
    | CabinetStanding of Cabinet : Entity * CabinetDoor : Entity

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

    member this.IsEnemyState =
        not this.IsPlayerState

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

    member this.BodyShapeTransform =
        match this with
        | Hunter | Stalker -> Affine.makeTranslation (v3 0.0f 0.9f 0.0f)
        | Player -> Affine.makeTranslation (v3 0.0f 0.75f 0.0f)

    member this.BodyShape =
        match this with
        | Hunter | Stalker -> CapsuleShape { Height = 1.0f; Radius = 0.4f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }
        | Player -> CapsuleShape { Height = 1.1f; Radius = 0.25f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

    member this.ExpandedHideSensorBodyShape =
        SphereShape { Radius = 2.0f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

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
    { AttackTime : GameTime
      AttackSoundPlayed : bool
      AttackedCharacters : Entity Set }

    static member make time =
        { AttackTime = time
          AttackSoundPlayed = false
          AttackedCharacters = Set.empty }

type InvestigateState =
    { Investigation : Entity }

type InjuryState =
    { InjuryTime : GameTime }

type WoundState =
    { WoundTime : GameTime
      WoundEventPublished : bool }

type ActionState =
    | NormalState
    | AttackState of AttackState
    | InvestigateState of InvestigateState
    | HideState of HideState
    | InjuryState of InjuryState
    | WoundState of WoundState