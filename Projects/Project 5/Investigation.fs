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

type InvestigationDispatcher () =
    inherit Entity3dDispatcherImNui (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.InvestigationPhase InvestigationNotStarted
         define Entity.InteractionResult Nothing]

    override this.Process (entity, world) =

        // declare multi-layer icon
        let world =
            let phase = entity.GetInvestigationPhase world
            let distanceScalar =
                if Simulants.GameplayPlayer.GetExists world then
                    let playerPosition = Simulants.GameplayPlayer.GetPosition world + v3Up * 1.25f
                    let playerDistance = playerPosition.Distance (entity.GetPosition world)
                    if playerDistance < 1.0f then 1.0f
                    elif playerDistance > 2.0f then 0.0f
                    else (2.0f - playerDistance) * 0.5f
                else 0.0f
            let visibility = (inc world.GameTime.Seconds % 2.0f) * distanceScalar / 2.0f
            let albedoImage =
                match phase with
                | InvestigationNotStarted -> Assets.Gameplay.InvestigationPendingIconAlbedoImage
                | InvestigationStarted _ -> Assets.Gameplay.InvestigationProcedingIconAlbedoImage
                | InvestigationFinished _ -> Assets.Gameplay.InvestigationConcludedIconAlbedoImage
            let emissionImage = Assets.Gameplay.IconEmissionImage
            let material = { Material.defaultMaterial with AlbedoImageOpt = ValueSome albedoImage; EmissionImageOpt = ValueSome emissionImage }
            let albedoColor = colorOne.WithA visibility
            let cels = if phase.IsInvestigationStarted then 8 else 1
            List.fold (fun (world : World) layer ->
                let albedoColor = albedoColor.MapA (fun a -> if layer = 0 then a * 0.8f else a * 0.2f)
                let materialProperties = { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome albedoColor }
                World.doAnimatedBillboard ("InvestigationIcon+" + string layer)
                    [Entity.Rotation @= quatIdentity
                     Entity.ScaleLocal .= v3Dup 0.1f
                     Entity.MaterialProperties @= materialProperties
                     Entity.Material @= material
                     Entity.RenderStyle .= Forward (0.0f, Single.MaxValue)
                     Entity.DepthTest .= if layer = 0 then LessThanOrEqualTest else AlwaysPassTest
                     Entity.AnimationDelay .= 1.0f
                     Entity.CelCount @= cels
                     Entity.CelRun @= cels] world)
                world [0 .. dec 2]

        // toggle based on result and advents
        let world =
            match entity.GetInteractionResult world with
            | FindDescription (_, advent)
            | FindItem (_, advent) ->
                let advents = Simulants.Gameplay.Get "Advents" world
                let visible = not (Set.contains advent advents)
                let world = entity.SetBodyEnabled visible world
                match entity.TryGetMountee world with
                | Some mountee -> mountee.SetVisible visible world
                | None -> world
            | EndGame -> world
            | Nothing -> world

        // fin
        world

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        if intersectionOpt.HasValue then [|intersectionOpt.Value|]
        else [||]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 1.0f) v3Zero