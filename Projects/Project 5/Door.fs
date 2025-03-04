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
        member this.GetClosable world : bool = this.Get (nameof this.Closable) world
        member this.SetClosable (value : bool) world = this.Set (nameof this.Closable) value world
        member this.Closable = lens (nameof this.Closable) this this.GetClosable this.SetClosable
        member this.GetRotationInitial world : Quaternion = this.Get (nameof this.RotationInitial) world
        member this.SetRotationInitial (value : Quaternion) world = this.Set (nameof this.RotationInitial) value world
        member this.RotationInitial = lens (nameof this.RotationInitial) this this.GetRotationInitial this.SetRotationInitial

type DoorDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (BoxShape { Size = v3Dup 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.DoorState DoorClosed
         define Entity.Closable true
         define Entity.RotationInitial quatIdentity]

    override this.PresenceOverride =
        ValueSome Exterior

    override this.Register (entity, world) =
        match entity.Parent with
        | :? Entity as parent -> entity.SetRotationInitial (parent.GetRotationLocal world) world
        | _ -> Log.warn "Door parent not an entity as intended."; world

    override this.Process (entity, world) =
        match entity.Parent with
        | :? Entity as parent -> 
            let rotationInitial = entity.GetRotationInitial world
            let progress =
                match entity.GetDoorState world with
                | DoorClosed -> 0.0f
                | DoorClosing startTime -> 1.0f - GameTime.progress startTime world.GameTime 1.25f
                | DoorOpened -> 1.0f
                | DoorOpening startTime -> GameTime.progress startTime world.GameTime 1.25f
            parent.SetRotationLocal (Quaternion.CreateFromAxisAngle (v3Up, -progress * 2.0f) * rotationInitial) world
        | _ -> world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.5f) v3Zero