namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module InvestigationDispatcherExtensions =
    type Entity with
        member this.GetInvestigationPhase world : InvestigationPhase = this.Get (nameof this.InvestigationPhase) world
        member this.SetInvestigationPhase (value : InvestigationPhase) world = this.Set (nameof this.InvestigationPhase) value world
        member this.InvestigationPhase = lens (nameof this.InvestigationPhase) this this.GetInvestigationPhase this.SetInvestigationPhase
        member this.GetInvestigationResult world : InvestigationResult = this.Get (nameof this.InvestigationResult) world
        member this.SetInvestigationResult (value : InvestigationResult) world = this.Set (nameof this.InvestigationResult) value world
        member this.InvestigationResult = lens (nameof this.InvestigationResult) this this.GetInvestigationResult this.SetInvestigationResult

type InvestigationDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.InvestigationPhase InvestigationNotStarted
         define Entity.InvestigationResult FindNothing]

    override this.Process (entity, world) =

        // declare multi-layer UI
        let world =
            List.fold (fun (world : World) layer ->
                let distanceScalar =
                    if Simulants.GameplayPlayer.GetExists world then
                        let playerPosition = Simulants.GameplayPlayer.GetPosition world + v3Up * 1.25f
                        let playerDistance = playerPosition.Distance (entity.GetPosition world)
                        if playerDistance < 1.0f then 1.0f
                        elif playerDistance > 2.0f then 0.0f
                        else (2.0f - playerDistance) * 0.5f
                    else 0.0f
                let alpha = (inc world.GameTime.Seconds % 2.0f) * distanceScalar / 2.0f * if layer = 0 then 1.0f else 0.2f
                let materialProperties = { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome (Color.White.WithA alpha) }
                let depthTest = if layer = 0 then LessThanOrEqualTest else AlwaysPassTest
                match entity.GetInvestigationPhase world with
                | InvestigationNotStarted ->
                    let material =
                        { Material.defaultMaterial with
                            AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationPendingIconAlbedoImage
                            EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
                    World.doStaticBillboard ("InvestigationNotStartedIcon+" + string layer)
                        [Entity.Rotation @= quatIdentity
                         Entity.ScaleLocal .= v3Dup 0.1f
                         Entity.MaterialProperties @= materialProperties
                         Entity.Material .= material
                         Entity.RenderStyle .= Forward (0.0f, Single.MaxValue)
                         Entity.DepthTest .= depthTest] world
                | InvestigationStarted _ ->
                    let material =
                        { Material.defaultMaterial with
                            AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationProcedingIconAlbedoImage
                            EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
                    World.doAnimatedBillboard ("InvestigationStartedIcon+" + string layer)
                        [Entity.Rotation @= quatIdentity
                         Entity.ScaleLocal .= v3Dup 0.1f
                         Entity.MaterialProperties @= materialProperties
                         Entity.Material .= material
                         Entity.AnimationDelay .= 1.0f
                         Entity.CelCount .= 8
                         Entity.CelRun .= 8
                         Entity.RenderStyle .= Forward (0.0f, Single.MaxValue)
                         Entity.DepthTest .= depthTest] world
                | InvestigationFinished _ -> world)
                world [0 .. dec 2]

        // try to make parent visibility match body enabled
        match entity.GetInvestigationResult world with
        | FindNothing -> world
        | FindDescription _ -> world
        | FindItem (_, advent) ->
            let advents = Simulants.Gameplay.Get "Advents" world
            let visible = not (Set.contains advent advents)
            let world = entity.SetBodyEnabled visible world
            match entity.TryGetMountee world with
            | Some mountee -> mountee.SetVisible visible world
            | None -> world

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        if intersectionOpt.HasValue then [|intersectionOpt.Value|]
        else [||]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 1.0f) v3Zero