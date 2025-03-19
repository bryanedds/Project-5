﻿namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type WayPointDispatcher () =
    inherit Entity3dDispatcherImNui (false, false, false)

    override this.RayCast (ray, entity, world) =
        base.RayCast (ray, entity, world)

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero