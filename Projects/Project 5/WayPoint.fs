namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type WayPointDispatcher () =
    inherit Entity3dDispatcherImNui (false, false, false)

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        if intersectionOpt.HasValue then [|intersectionOpt.Value|]
        else [||]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero