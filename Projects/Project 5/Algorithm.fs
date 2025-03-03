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

    let getTargetsInSight sightDistance position rotation bodyId targetIds world =
        let targetSightings =
            seq {
                for scanSegment in computeScanSegments sightDistance position rotation do
                    let scannedOpt =
                        World.rayCast3dBodies scanSegment Int32.MaxValue false world |>
                        Seq.filter (fun intersection -> intersection.BodyShapeIntersected.BodyId <> bodyId) |>
                        //Seq.filter (fun intersection -> not (World.getBodySensor intersection.BodyShapeIntersected.BodyId world)) |>
                        Seq.tryHead
                    match scannedOpt with
                    | Some scanned when Set.contains scanned.BodyShapeIntersected.BodyId targetIds -> true
                    | Some _ | None -> () }
        Seq.notEmpty targetSightings