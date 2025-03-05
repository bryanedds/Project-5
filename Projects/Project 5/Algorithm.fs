namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<RequireQualifiedAccess>]
module Algorithm =

    let computeScanSegments (sightDistance : single) position rotation =
        let sightPosition = position + v3Up * 1.25f
        let sightRotation = rotation
        seq {
            for i in 0 .. dec 15 do
                let angle = Quaternion.CreateFromAxisAngle (v3Up, single i * 5.0f - 35.0f |> Math.DegreesToRadians)
                let scanRotation = sightRotation * angle
                Segment3 (sightPosition, sightPosition + scanRotation.Forward * sightDistance) }

    let getTargetInSight sightDistance position rotation bodyId targetIds world =
        let targetSightings =
            seq {
                for scanSegment in computeScanSegments sightDistance position rotation do
                    let scannedOpt =
                        World.rayCast3dBodies scanSegment Int32.MaxValue false world |>
                        Seq.filter (fun intersection -> intersection.BodyShapeIntersected.BodyId <> bodyId) |>
                        Seq.filter (fun intersection -> intersection.BodyShapeIntersected.BodyId.BodySource.Name <> Constants.Gameplay.CharacterWeaponName) |>
                        Seq.tryHead
                    match scannedOpt with
                    | Some scanned when Set.contains scanned.BodyShapeIntersected.BodyId targetIds -> true
                    | Some _ | None -> () }
        Seq.notEmpty targetSightings

    let computeEyeDistanceScalar position (rotation : Quaternion) actionState (entity : Entity) (world : World) =
        let eyeDistanceScalarA = ActionState.computeEyeDistanceScalar world.GameTime actionState
        let positionEyeLevel = position + v3Up * Constants.Gameplay.PlayerEyeLevel
        let segment = Segment3 (positionEyeLevel, positionEyeLevel + rotation.Back)
        let entityEhs = entity / Constants.Gameplay.CharacterExpandedHideSensorName
        let eyeDistanceScalarBOpt =
            if entity = Simulants.GameplayPlayer then
                World.rayCast3dBodies segment Int32.MaxValue false world |>
                Seq.filter (fun intersection -> not (World.getBodySensor intersection.BodyShapeIntersected.BodyId world)) |>
                Seq.filter (fun intersection -> intersection.BodyShapeIntersected.BodyId.BodySource <> entity) |>
                Seq.filter (fun intersection -> intersection.BodyShapeIntersected.BodyId.BodySource <> entityEhs) |>
                Seq.choose (fun intersection -> match tryCast<Entity> intersection.BodyShapeIntersected.BodyId.BodySource with Some entity -> Some (intersection.Progress, entity) | None -> None) |>
                Seq.map fst |>
                Seq.tryHead
            else None
        let eyeDistanceScalarMin =
            match eyeDistanceScalarBOpt with
            | Some eyeDistanceScalarB -> min eyeDistanceScalarA eyeDistanceScalarB
            | None -> eyeDistanceScalarA
        max 0.0f (eyeDistanceScalarMin - 0.1f)

    let computeVisibilityScalar position rotation actionState entity world =
        computeEyeDistanceScalar position rotation actionState entity world * (1.0f / 0.9f)