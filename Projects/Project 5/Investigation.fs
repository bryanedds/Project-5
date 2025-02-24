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

    static member Properties =
        [define Entity.InvestigationPhase InvestigationNotStarted]

    override this.Process (entity, world) =
        match entity.GetInvestigationPhase world with
        | InvestigationNotStarted ->
            let materialProperties =
                { MaterialProperties.defaultProperties with
                    AlbedoOpt = ValueSome (Color.White.WithA (inc world.GameTime.Seconds % 2.0f)) }
            let material =
                { Material.defaultMaterial with
                    AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationPendingIconAlbedoImage // TODO: concluded if successful investigation outcome already
                    EmissionImageOpt = ValueSome Assets.Gameplay.IconEmissionImage }
            World.doStaticBillboard "InvestigationNotStartedIcon"
                [Entity.PositionLocal .= v3 0.0f 0.0f 0.1f
                 Entity.ScaleLocal .= v3Dup 0.1f
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
                [Entity.PositionLocal .= v3 0.0f 0.0f 0.1f
                 Entity.ScaleLocal .= v3Dup 0.1f
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
                [Entity.PositionLocal .= v3 0.0f 0.0f 0.1f
                 Entity.ScaleLocal .= v3Dup 0.1f
                 Entity.Material .= material
                 Entity.AnimationDelay .= 1.0f
                 Entity.CelCount .= 1
                 Entity.CelRun .= 1
                 Entity.RenderStyle .= Forward (0.0f, Single.MinValue)]
                world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero