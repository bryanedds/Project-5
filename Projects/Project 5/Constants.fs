namespace MyGame
open System
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"
        let [<Literal>] EnemySightDistance = 8.0f
        let [<Literal>] HuntDuration = 15.0f
        let [<Literal>] StalkDelay = 180.0f
        let [<Literal>] StalkDuration = 90.0f

        