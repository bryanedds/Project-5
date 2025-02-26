namespace MyGame
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"
        let [<Literal>] EnemySightDistance = 8.0f
        let [<Uniform>] HuntDuration = GameTime.ofSeconds 15.0f
        let [<Uniform>] StalkDelay = GameTime.ofSeconds 180.0f
        let [<Uniform>] StalkDuration = GameTime.ofSeconds 90.0f