﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type SpiritMovement =
    | Creep
    | Chase
    | Scatter
    | Wander

type SpiritPattern =
    | Confused
    | Flighty
    | Steadfast
    | Stalking
    | Ambushing

    static member ofInt i =
        match i with
        | 0 -> Confused
        | 1 -> Flighty
        | 2 -> Steadfast
        | 3 -> Stalking
        | 4 -> Ambushing
        | _ -> failwithumf ()

    static member generate () =
        if Gen.randomb // 50% less chance of Stalking and Ambushing spirit
        then Gen.random1 5 |> SpiritPattern.ofInt
        else Gen.random1 3 |> SpiritPattern.ofInt

    static member toSpiritMovement pattern =
        match pattern with
        | Confused ->       [|Creep;    Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander; Wander|]
        | Flighty ->        [|Chase;    Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander;   Creep; Creep; Wander|]
        | Steadfast ->      [|Creep;    Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep;    Creep; Creep; Creep|]
        | Stalking ->       [|Creep;    Chase; Chase; Creep;    Chase; Chase; Creep;    Chase; Chase; Creep;    Chase; Chase; Creep;    Chase; Chase; Creep|]
        | Ambushing ->      [|Chase;    Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase;    Chase; Chase; Chase|]

type SpiritState =
    { SpiritMovements : SpiritMovement array
      SpiritMovementIndex : int
      SpiritMovementStart : int64
      SpiritMovementCachedOpt : Vector3 option }

    static member update time (position : Vector3) (target : Vector3) spiritState =
        let localTime = time - spiritState.SpiritMovementStart
        let spiritState =
            if localTime >= Constants.Field.SpiritMovementDuration then
                let index = inc spiritState.SpiritMovementIndex
                { spiritState with
                    SpiritMovementIndex = if index >= Array.length spiritState.SpiritMovements then 0 else index
                    SpiritMovementStart = time
                    SpiritMovementCachedOpt = None }
            else spiritState
        match spiritState.SpiritMovements.[spiritState.SpiritMovementIndex] with
        | Creep ->
            let v = target - position
            let n = v.Normalized
            let m = n * Constants.Field.SpiritWalkSpeed
            (m, spiritState)
        | Chase ->
            let v = target - position
            let n = v.Normalized
            let m = n * Constants.Field.SpiritRunSpeed
            (m, spiritState)
        | Scatter ->
            match spiritState.SpiritMovementCachedOpt with
            | None ->
                let v = target - position
                let n = v.Normalized
                let m = n * Constants.Field.SpiritWalkSpeed
                let spiritState = { spiritState with SpiritMovementCachedOpt = Some m }
                (m, spiritState)
            | Some v -> (v, spiritState)
        | Wander ->
            match spiritState.SpiritMovementCachedOpt with
            | None ->
                let r = Gen.randomd * Math.PI * 2.0
                let n = v3 (single (cos r)) (single (sin r)) 0.0f
                let m = n * Constants.Field.SpiritWalkSpeed
                let spiritState = { spiritState with SpiritMovementCachedOpt = Some m }
                (m, spiritState)
            | Some v -> (v, spiritState)

[<RequireQualifiedAccess>]
module Spirit =

    type Spirit =
        { Perimeter : Box3
          SpiritType : SpiritType
          SpiritState : SpiritState }

        static member update time target (spirit : Spirit) =
            let (movement, state) = SpiritState.update time spirit.Perimeter.Center target spirit.SpiritState
            { spirit with
                Perimeter = spirit.Perimeter.Translate movement
                SpiritState = state }

        static member spawn time center spiritType spiritPattern =
            let r = Gen.randomd * Math.PI * 2.0
            let n = v3 (single (cos r)) (single (sin r)) 0.0f
            let p = center + n * Constants.Field.SpiritRadius
            let spiritState = { SpiritMovements = spiritPattern; SpiritMovementIndex = 0; SpiritMovementStart = time; SpiritMovementCachedOpt = None }
            { Perimeter = box3 p Constants.Field.SpiritOrbBlipSize; SpiritType = spiritType; SpiritState = spiritState }

        static member make bounds spiritType spiritState =
            { Perimeter = bounds
              SpiritType = spiritType
              SpiritState = spiritState }

type Spirit = Spirit.Spirit