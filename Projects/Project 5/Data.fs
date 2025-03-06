namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type ItemType =
    | BronzeKey

type [<SymbolicExpansion>] Inventory =
    { Items : Map<ItemType, int>
      Currency : int }

    static member empty =
        { Items = Map.empty
          Currency = 0 }

    static member initial =
        { Items = Map.empty
          Currency = 10 }

type WayPoint =
    { WayPoint : Entity Relation
      WayPointWaitTime : GameTime }

type Awareness =
    | UnawareOfTarget
    | AwareOfTargetTraversing of GameTime
    | AwareOfTargetHiding of GameTime

type [<SymbolicExpansion>] HunterState =
    { HunterWayPoints : WayPoint array
      HunterWayPointPlayback : Playback
      HunterWayPointBouncing : bool
      HunterWayPointIndexOpt : int option
      HunterWayPointTimeOpt : GameTime option
      HunterAwareness : Awareness }

    static member initial =
        { HunterWayPoints = [||]
          HunterWayPointPlayback = Loop
          HunterWayPointBouncing = false
          HunterWayPointIndexOpt = None
          HunterWayPointTimeOpt = None
          HunterAwareness = UnawareOfTarget }

type StalkingState =
    { SpawnPosition : Vector3
      Awareness : Awareness }

type LeavingState =
    { UnspawnPosition : Vector3
      Awareness : Awareness }

type [<SymbolicExpansion>] StalkerState =
    | IdlingState
    | StalkingState of StalkingState
    | LeavingState of LeavingState

    static member initial = IdlingState

type StalkerSpawnState =
    | StalkerUnspawned of UnspawnTime : GameTime
    | StalkerStalking of CaughtTargetHiding : bool * SpawnPoint : Entity * SpawnTime : GameTime
    | StalkerLeaving of UnspawnPoint : Entity * SpawnTime : GameTime

    member this.SpawnTimeOpt =
        match this with
        | StalkerUnspawned _ -> None
        | StalkerStalking (_, _, spawnTime) -> Some spawnTime
        | StalkerLeaving (_, spawnTime) -> Some spawnTime

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
    | HideEmerging
    | HideUncovered

type HidingSpot =
    | CabinetStanding of Cabinet : Entity * CabinetDoor : Entity

type HideState =
    { HideTime : GameTime
      HidePhase : HidePhase }

type CharacterState =
    | HunterState of HunterState
    | StalkerState of StalkerState
    | PlayerState

    member this.IsEnemyState =
        not this.IsPlayerState

    member this.CharacterType =
        match this with
        | HunterState _ -> Hunter
        | StalkerState _ -> Stalker
        | PlayerState -> Player

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
        | Player -> Assets.Gameplay.AvaModel

    member this.AnimationRate =
        match this with
        | Hunter | Stalker -> 1.0f
        | Player -> 2.0f

    member this.SubsortOffsets =
        match this with
        | Hunter | Stalker -> Map.empty
        | Player -> Map.ofList [(0, -1.0f); (1, -1.0f); (2, -1.0f); (3, -1.0f); (6, 1.0f); (7, 1.0f)]

    member this.BodyShapeTransform =
        match this with
        | Hunter | Stalker -> Affine.makeTranslation (v3 0.0f 1.1f 0.0f)
        | Player -> Affine.makeTranslation (v3 0.0f 0.75f 0.0f)

    member this.BodyShape =
        match this with
        | Hunter | Stalker -> CapsuleShape { Height = 1.0f; Radius = 0.6f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }
        | Player -> CapsuleShape { Height = 1.1f; Radius = 0.25f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

    member this.ExpandedHideSensorBodyShape =
        SphereShape { Radius = 1.0f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

    member this.CharacterProperties =
        match this with
        | Hunter | Stalker -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f }
        | Player -> CharacterProperties.defaultProperties

    member this.InitialState =
        match this with
        | Hunter -> HunterState HunterState.initial
        | Stalker -> StalkerState StalkerState.initial
        | Player -> PlayerState

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

    static member computeEyeDistanceScalar time state =
        match state with
        | HideState hide ->
            match hide.HidePhase with
            | HideEntering -> 1.0f - GameTime.progress hide.HideTime time 1.5f
            | HideEmerging -> GameTime.progress hide.HideTime time 1.5f
            | HideWaiting -> 0.0f
            | HideUncovered -> 0.0f
        | _ -> 1.0f

    static member computeVisibilityScalar time state =
        let eyeDistanceScalar = ActionState.computeEyeDistanceScalar time state
        let eyeDistanceScalar =
            if eyeDistanceScalar > Constants.Gameplay.PlayerVisibilityDistanceMin
            then (eyeDistanceScalar - Constants.Gameplay.PlayerVisibilityDistanceMin) * (1.0f / Constants.Gameplay.PlayerVisibilityDistanceMin) + 0.0001f
            else 0.0f
        saturate eyeDistanceScalar