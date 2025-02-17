namespace MyGame
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package. Note that we don't actually have any assets here yet, but they can be
    // added to the existing package at your leisure!
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"
        let SlashSound = asset<Sound> PackageName "Slash"
        let Slash2Sound = asset<Sound> PackageName "Slash2"
        let InjureSound = asset<Sound> PackageName "Injure"
        let AmbienceSong = asset<Song> PackageName "Ambience"
        let AstrolabeSong = asset<Song> PackageName "Astrolabe"
        let HeartFull = asset<Image> PackageName "HeartFull"
        let HeartEmpty = asset<Image> PackageName "HeartEmpty"
        let SophieModel = asset<AnimatedModel> PackageName "Sophie"
        let RhyoliteModel = asset<AnimatedModel> PackageName "Rhyolite"
        let CruciformModel = asset<AnimatedModel> PackageName "Cruciform"
        let GreatSwordModel = asset<StaticModel> PackageName "GreatSword"