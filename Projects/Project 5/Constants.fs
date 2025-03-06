namespace MyGame
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterExpandedHideSensorName = "ExpandedHideSensor"
        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"
        let [<Literal>] EnemySightDistance = 7.5f
        let [<Uniform>] AttackProximity = 0.5f
        let [<Uniform>] HuntWayPointProximity = 0.5f
        let [<Uniform>] AwareOfTargetTraversingDuration = GameTime.ofSeconds 16.0f
        let [<Uniform>] AwareOfTargetHidingDuration = GameTime.ofSeconds 32.0f
        let [<Uniform>] StalkDelay = GameTime.ofSeconds 180.0f
        let [<Uniform>] StalkDuration = GameTime.ofSeconds 120.0f
        let [<Uniform>] PlayerEyeLevel = 1.5f