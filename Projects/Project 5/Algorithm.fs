namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<RequireQualifiedAccess>]
module Algorithm =

    let computeScanSegments position rotation (sightDistance : single) =
        let sightPosition = position + v3Up * 1.25f
        let sightRotation = rotation
        seq {
            for i in 0 .. dec 15 do
                let angle = Quaternion.CreateFromAxisAngle (v3Up, single i * 5.0f - 35.0f |> Math.DegreesToRadians)
                let scanRotation = sightRotation * angle
                Segment3 (sightPosition, sightPosition + scanRotation.Forward * sightDistance) }

    let getTargetInSight position rotation sightDistance targetId world =
        let targetSightings =
            seq {
                for scanSegment in computeScanSegments position rotation sightDistance do
                    let intersected = World.rayCast3dBodies scanSegment Int32.MaxValue false world
                    if  intersected.Length > 1 &&
                        intersected.[1].BodyShapeIntersected.BodyId = targetId then
                        true }
        Seq.notEmpty targetSightings