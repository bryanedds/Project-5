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
        let [<Literal>] EnemySightDistance = 7.5f
        let [<Uniform>] AttackProximity = 0.5f
        let [<Uniform>] HuntWayPointProximity = 0.5f
        let [<Uniform>] HuntDuration = GameTime.ofSeconds 16.0f
        let [<Uniform>] StalkDelay = GameTime.ofSeconds 180.0f
        let [<Uniform>] StalkDuration = GameTime.ofSeconds 90.0f