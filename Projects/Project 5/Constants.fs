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
        let [<Literal>] CharacterLightName = "Light"
        let [<Literal>] EnemySightDistance = 7.5f
        let [<Uniform>] AttackProximity = 0.5f
        let [<Uniform>] HuntWayPointProximity = 0.5f
        let [<Uniform>] StepSoundDistanceScalar = 0.05f
        let [<Uniform>] AwareOfTargetTraversingDuration = GameTime.ofSeconds 16.0
        let [<Uniform>] AwareOfTargetHidingDuration = GameTime.ofSeconds 32.0
        let [<Uniform>] StalkDelay = GameTime.ofSeconds 180.0
        let [<Uniform>] StalkDuration = GameTime.ofSeconds 90.0
        let [<Uniform>] PlayerEyeLevel = 1.5f
        let [<Uniform>] PlayerEyeDistance = 0.9f
        let [<Uniform>] PlayerEyeShiftAngle = 0.2f
        let [<Uniform>] PlayerVisibilityDistanceMin = 0.55f