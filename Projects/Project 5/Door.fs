namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module DoorDispatcherExtensions =
    type Entity with
        member this.GetDoorState world : DoorState = this.Get (nameof this.DoorState) world
        member this.SetDoorState (value : DoorState) world = this.Set (nameof this.DoorState) value world
        member this.DoorState = lens (nameof this.DoorState) this this.GetDoorState this.SetDoorState

type DoorDispatcher () =
    inherit Entity3dDispatcherImNui (false, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Sensor true
         define Entity.DoorState DoorClosed]

    override this.PresenceOverride =
        ValueSome Exterior

    override this.Process (entity, world) =
        match entity.GetDoorState world with
        | DoorClosed -> world
        | DoorClosing startTime ->
            let progress = 1.0f - GameTime.progress startTime world.GameTime 1.25f
            (entity.Parent :?> Entity).SetRotationLocal (Quaternion.CreateFromAxisAngle (v3Up, -progress * 2.0f)) world
        | DoorOpened -> world
        | DoorOpening startTime ->
            let progress = GameTime.progress startTime world.GameTime 1.25f
            (entity.Parent :?> Entity).SetRotationLocal (Quaternion.CreateFromAxisAngle (v3Up, -progress * 2.0f)) world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero