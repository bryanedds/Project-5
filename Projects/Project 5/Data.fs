namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

(* Inventory *)

type ItemType =
    | BlackKey // just for testing
    | BronzeKey

type [<SymbolicExpansion>] Inventory =
    Map<ItemType, int>

[<RequireQualifiedAccess>]
module Inventory =

    let empty =
        Map.empty

    let initial =
        Map.singleton BlackKey 1

(* Advents *)

type Advent =
    | Narrated of string
    | Found of ItemType
    | Inserted of ItemType

(* Interaction *)

type InteractionResult =
    | Description of string
    | Narration of string
    | Find of ItemType
    | FindNonUnique of ItemType * Advent
    | EndGame
    | Nothing

(* Movement *)

type MovementState =
    | Standing of GameTime
    | Walking of startTime : GameTime * lastStepTime : GameTime

(* Action *)

type AttackState =
    { AttackTime : GameTime
      AttackSoundPlayed : bool
      AttackedCharacters : Entity Set }

    static member make time =
        { AttackTime = time
          AttackSoundPlayed = false
          AttackedCharacters = Set.empty }

type InsertionPhase =
    | InsertionNotStarted
    | InsertionStarted of StartTime : GameTime
    | InsertionFinished

type InsertionState =
    { InsertionSpot : Entity }

type InvestigationPhase =
    | InvestigationNotStarted
    | InvestigationStarted of GameTime
    | InvestigationFinished of GameTime

type InvestigationState =
    { InvestigationSpot : Entity }

type InjuryState =
    { InjuryTime : GameTime }

type HideType =
    | HideStanding // like a locker or tall cupboard
    | HideKneeling // like in a floor cabinet
    | HideLying // like under a bed

type HidePhase =
    | HideEntering
    | HideWaiting
    | HideEmerging
    | HideUncovered

type HideState =
    { HideTime : GameTime
      HidePhase : HidePhase }

type HidingSpot =
    | CabinetStanding of Cabinet : Entity * CabinetDoor : Entity

type WoundState =
    { WoundTime : GameTime
      WoundEventPublished : bool }

type ActionState =
    | NormalState
    | AttackState of AttackState
    | InventoryState
    | InsertionState of InsertionState
    | InvestigationState of InvestigationState
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

(* Way Point *)

type WayPoint =
    { WayPoint : Entity Relation
      WayPointWaitTime : GameTime }

(* Door *)

type DoorState =
    | DoorClosed
    | DoorOpening of StartTime : GameTime
    | DoorOpened
    | DoorClosing of StartTime : GameTime

(* Hunter *)

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

(* Stalker *)

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

(* Player *)

type PlayerState =
    { ViewFlip : bool
      FlashLightEnabled : bool }

    static member initial =
        { ViewFlip = false
          FlashLightEnabled = true }

(* Character *)

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
        | Player -> Assets.Gameplay.AvaModel

    member this.AnimationRate =
        match this with
        | Hunter | Stalker -> 1.0f
        | Player -> 2.0f

    member this.SubsortOffsets =
        match this with
        | Hunter | Stalker -> Map.empty
        | Player -> Map.ofList [(3, -1.0f); (8, -2.0f); (14, -1.0f)]

    member this.BodyShapeTransform =
        match this with
        | Hunter | Stalker -> Affine.makeTranslation (v3 0.0f 0.8f 0.0f)
        | Player -> Affine.makeTranslation (v3 0.0f 0.8f 0.0f)

    member this.BodyShape =
        match this with
        | Hunter | Stalker -> CapsuleShape { Height = 1.0f; Radius = 0.3f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }
        | Player -> CapsuleShape { Height = 1.1f; Radius = 0.25f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

    member this.ExpandedHideSensorBodyShape =
        SphereShape { Radius = 1.0f; TransformOpt = Some this.BodyShapeTransform; PropertiesOpt = None }

    member this.CharacterProperties =
        match this with
        | Hunter | Stalker -> { CharacterProperties.defaultProperties with CollisionTolerance = 0.005f } // NOTE: I think this is to make enemies able to climb stairs, but now I'm not sure I remember...
        | Player -> { CharacterProperties.defaultProperties with StairStepUp = v3 0.0f 0.125f 0.0f; StairStepDownStickToFloor = v3 0.0f -0.125f 0.0f }

    member this.InitialState =
        match this with
        | Hunter -> HunterState HunterState.initial
        | Stalker -> StalkerState StalkerState.initial
        | Player -> PlayerState PlayerState.initial

(* Prelude *)

type BooleanOperation =
    | BooleanUnion
    | BooleanDifference
    | BooleanExclusive
    | BooleanIntersection

type DiscreteOperation =
    | DiscreteOperation of (int -> Vector3 (*position*) -> Vector3 (*normal*) -> Matrix4x4 -> World -> World)
    | CreateEntity of EntityDescriptor
    | Nop

type 'a Bounded =
    | Included of 'a
    | Excluded

type DiscreteMapping = int -> DiscreteOperation Bounded

(* Discrete Space *)

type DiscreteSpace =
    | DiscreteSpace of ((Vector3 -> bool) * (unit -> Vector3 array))
    | Points of Vector3 array

type DiscreteDiscretize =
    | DiscreteDiscretize of (DiscreteSpace -> DiscreteMapping)
    | DiscreteIdentity // trivial identity mapping

type DiscreteUnop =
    | DiscreteUnop of (DiscreteSpace -> DiscreteSpace)
    | DiscreteTransformation of Affine

type DiscreteBinop =
    | DiscreteBinop of (DiscreteSpace -> DiscreteSpace -> DiscreteSpace)
    | DiscreteBooleanOperation of BooleanOperation

(* Linear Space *)

type LinearSpace =
    | LinearSpace of ((Vector3 -> bool) * (unit -> Vector3 array))
    | Spline of Vector3 array

type LinearDiscretize =
    | LinearDiscretize of (LinearSpace -> DiscreteMapping)
    | LinearSegmentation of int

type LinearUnop =
    | LinearUnop of (LinearSpace -> LinearSpace)
    | LinearTransformation of Affine

type LinearBinop =
    | LinearBinop of (LinearSpace -> LinearSpace -> LinearSpace)
    | LinearConcat // concatenates two linear spaces

(* Euclidean Space *)

type EuclideanSpace =
    | EuclideanSpace of ((Vector3 -> bool) * (unit -> Vector3 array))
    | EuclideanBox of Box3
    | EuclideanPlane of Plane3
    | EuclideanSphere of Sphere

type EuclideanDiscretize =
    | EuclideanDiscretize of (EuclideanSpace -> DiscreteMapping)
    | EuclideanGrid of Vector3

type EuclideanUnop =
    | EuclideanUnop of (EuclideanSpace -> EuclideanSpace)
    | EuclideanTransformation of Affine

type EuclideanBinop =
    | EuclideanBinop of (EuclideanSpace -> EuclideanSpace -> EuclideanSpace)
    | EuclideanBooleanOperation of BooleanOperation

(* Topography Space *)

type TopographySpace =
    | TopographySpace of ((Vector3 -> bool) * (unit -> Vector3 array))
    | HeightMap of HeightMap * Matrix4x4
    | StaticModel of StaticModel AssetTag * Matrix4x4
    | StaticModelSurface of int * StaticModel AssetTag * Matrix4x4
    | StaticModelEntity of Entity
    | StaticModelSurfaceEntity of Entity
    | RigidBodyEntity of Entity

type TopographyDiscretize =
    | TopographyDiscretize of (TopographySpace -> DiscreteMapping)
    | TopographyBarycentric of Vector2
    | Centers // take center of triangles, including for height field triangles
    | Points // take unique points of triangles, including for height field triangles

type TopographyUnop =
    | TopographyUnop of (TopographySpace -> TopographySpace)
    | TopographyTransformation of Affine

type TopographyBinop =
    | TopographyBinop of (TopographySpace -> TopographySpace -> TopographySpace)
    | TopographyConcat // concatenates two topography spaces

type TopographyToEuclidean = TopographySpace -> EuclideanSpace

type Space =
    | DiscreteSpace of DiscreteSpace
    | LinearSpace of LinearSpace
    | EuclideanSpace of EuclideanSpace
    | TopographySpace of TopographySpace
    | Spaces of Space array

type SpaceBinop =
    | SpaceBinop of (Space -> Space -> Space)
    | Stamp of (DiscreteSpace -> Space -> Space) // duplicates a space at every discrete point in some way
    | StampAffine of Affine // duplicates a space at every discrete point with the given affine transformation
    | StampIdentity // duplicates a space at every discrete point with identity transformation
    | Branch of (LinearSpace -> Space -> Space) // duplicates a space at every linear segment in some way
    | BranchAffine of Affine
    | BranchIdentity

(* Placement *)

type PlacementObject = string // like "Desk" or "Apple" or "Book" or "Utensil"

type PlacementPlatform =
    | PlacementPlatform of ((*Normal*) Vector3 -> bool)
    | TopUp of AllowOverhang : bool // like on top of a box
    | BottomUp of ExcludingSides : bool // like inside a box
    | Anywhere
    | Nowhere

type PlacementAttribute =
    | ConvexPlatform of Vector3 array * PlacementPlatform // uses triangle list
    | BoxPlatform of Box3 * PlacementPlatform
    | QuadPlatform of Vector3 * Vector3 * Vector3 * Vector3 * PlacementPlatform
    | BoundsPlatform of PlacementPlatform // uses existing spatial bounds rather than user-defined bounds

type PlacementWeight =
    | Noise of (Vector3 -> single)
    | Perlin of Scale : single
    | WeightImage of Scale : single * Image AssetTag

type PlacementRule =
    | Stack of PlacementObject * PlacementObject // can set right on left

type PlacementDistribution =
    | PlacementDistribution of PlacementObject * PlacementWeight

type PlacementDefinitions =
    { Rules : PlacementRule array
      Attributes : Map<PlacementObject, PlacementAttribute array>
      Distributions : PlacementDistribution array }