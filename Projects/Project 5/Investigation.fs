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
    inherit Entity3dDispatcherImNui (false, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Sensor true
         define Entity.InvestigationPhase InvestigationNotStarted]

    override this.PresenceOverride =
        ValueSome Exterior

    override this.Process (entity, world) =
        match entity.GetInvestigationPhase world with
        | InvestigationNotStarted ->
            let distanceScalar =
                if Simulants.GameplayPlayer.GetExists world then
                    let playerPosition = Simulants.GameplayPlayer.GetPosition world + v3Up * 1.25f
                    let playerDistance = playerPosition.Distance (entity.GetPosition world)
                    if playerDistance < 1.0f then 1.0f
                    elif playerDistance > 3.0f then 0.0f
                    else (3.0f - playerDistance) * 0.5f
                else 0.0f
            let alpha = (2.0f - world.GameTime.Seconds % 2.0f) * distanceScalar
            let materialProperties = { MaterialProperties.defaultProperties with AlbedoOpt = ValueSome (Color.White.WithA alpha) }
            let material =
                { Material.defaultMaterial with
                    AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationPendingIconAlbedoImage // TODO: concluded if successful investigation outcome already
                    EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
            World.doStaticBillboard "InvestigationNotStartedIcon"
                [Entity.ScaleLocal .= v3Dup 0.1f
                 Entity.MaterialProperties @= materialProperties
                 Entity.Material .= material
                 Entity.RenderStyle .= Forward (0.0f, Single.MinValue)]
                world
        | InvestigationStarted ->
            let material =
                { Material.defaultMaterial with
                    AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationProcedingIconAlbedoImage
                    EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
            World.doAnimatedBillboard "InvestigationStartedIcon"
                [Entity.ScaleLocal .= v3Dup 0.1f
                 Entity.Material .= material
                 Entity.AnimationDelay .= 1.0f
                 Entity.CelCount .= 8
                 Entity.CelRun .= 8
                 Entity.RenderStyle .= Forward (0.0f, Single.MinValue)]
                world
        | InvestigationFinished ->
            let material =
                { Material.defaultMaterial with
                    AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationConcludedIconAlbedoImage
                    EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
            World.doAnimatedBillboard "InvestigationConcludedIcon"
                [Entity.ScaleLocal .= v3Dup 0.1f
                 Entity.Material .= material
                 Entity.AnimationDelay .= 1.0f
                 Entity.CelCount .= 1
                 Entity.CelRun .= 1
                 Entity.RenderStyle .= Forward (0.0f, Single.MinValue)]
                world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero