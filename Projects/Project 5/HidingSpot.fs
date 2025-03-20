﻿namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module HidingSpotDispatcherExtensions =
    type Entity with
        member this.GetHidingSpotOpt world : HidingSpot option = this.Get (nameof this.HidingSpotOpt) world
        member this.SetHidingSpotOpt (value : HidingSpot option) world = this.Set (nameof this.HidingSpotOpt) value world
        member this.HidingSpotOpt = lens (nameof this.HidingSpotOpt) this this.GetHidingSpotOpt this.SetHidingSpotOpt

type HidingSpotDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (BoxShape { Size = v3Dup 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.HidingSpotOpt None]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.5f) v3Zero