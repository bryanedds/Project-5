namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

[<AutoOpen>]
module InvestigatableFacetExtensions =
    type Entity with
        member this.GetInvestigationPhase world : InvestigationPhase = this.Get (nameof this.InvestigationPhase) world
        member this.SetInvestigationPhase (value : InvestigationPhase) world = this.Set (nameof this.InvestigationPhase) value world
        member this.InvestigationPhase = lens (nameof this.InvestigationPhase) this this.GetInvestigationPhase this.SetInvestigationPhase

type InvestigatableFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.AlwaysUpdate true
         define Entity.InvestigationPhase InvestigationNotStarted]

    override this.Register (entity, world) =
        let icon = entity / "Icon"
        let world = World.createEntity<AnimatedBillboardDispatcher> DefaultOverlay (Some icon.Surnames) entity.Group world |> snd
        let world = icon.Material.Map (fun material -> { material with AlbedoImageOpt = ValueSome Assets.Gameplay.InvestigationIconImage }) world
        let world = icon.SetAnimationDelay 1.0f world
        let world = icon.SetCelCount 8 world
        let world = icon.SetCelRun 8 world
        let world = icon.SetRenderStyle (Forward (0.0f, Single.MinValue)) world
        let world = icon.SetPositionLocal (v3 0.0f -0.25f 0.1f) world
        let world = icon.SetScaleLocal (v3Dup 0.2f) world
        let world = icon.SetMountOpt (Some (Relation.makeParent ())) world
        world

    override this.Update (entity, world) =
        let icon = entity / "Icon"
        match entity.GetInvestigationPhase world with
        | InvestigationNotStarted -> icon.SetStartTime world.GameTime world
        | InvestigationStarted -> world
        | InvestigationFinished-> icon.SetStartTime (world.GameTime - GameTime.ofSeconds 8.0f) world
        