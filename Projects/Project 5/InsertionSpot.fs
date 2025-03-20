namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module InsertionSpotDispatcherExtensions =
    type Entity with
        member this.GetInsertionPhase world : InsertionPhase = this.Get (nameof this.InsertionPhase) world
        member this.SetInsertionPhase (value : InsertionPhase) world = this.Set (nameof this.InsertionPhase) value world
        member this.InsertionPhase = lens (nameof this.InsertionPhase) this this.GetInsertionPhase this.SetInsertionPhase
        member this.GetInsertionKey world : ItemType = this.Get (nameof this.InsertionKey) world
        member this.SetInsertionKey (value : ItemType) world = this.Set (nameof this.InsertionKey) value world
        member this.InsertionKey = lens (nameof this.InsertionKey) this this.GetInsertionKey this.SetInsertionKey
        member this.GetInteractionResult world : InteractionResult = this.Get (nameof this.InteractionResult) world
        member this.SetInteractionResult (value : InteractionResult) world = this.Set (nameof this.InteractionResult) value world
        member this.InteractionResult = lens (nameof this.InteractionResult) this this.GetInteractionResult this.SetInteractionResult

type InsertionSpotDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.InsertionPhase InsertionNotStarted
         define Entity.InsertionKey BlackKey
         define Entity.InteractionResult Nothing]

    override this.Process (entity, world) =
        match entity.GetInsertionPhase world with
        | InsertionNotStarted -> world
        | InsertionStarted startTime ->
            let localTime = world.GameTime - startTime
            if localTime > 4.0f
            then entity.SetInsertionPhase InsertionFinished world
            else world
        | InsertionFinished -> world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 1.0f) v3Zero