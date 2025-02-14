﻿namespace MyGame
open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImNui ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (screenResults, gameplay, world) =

        // only process when selected
        if gameplay.GetSelected world then

            // begin scene declaration
            let initializing = FQueue.contains Select screenResults
            let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

            // declare player
            let world =
                World.doEntity<PlayerDispatcher> "Player"
                    [if initializing then Entity.Position @= v3 1.0f 0.0f -1.0f
                     Entity.Size .= v3 1.5f 2.0f 1.5f
                     Entity.Offset .= v3 0.0f 1.0f 0.0f
                     Entity.AnimatedModel .= Assets.Gameplay.Sophie] world
            let player = world.RecentEntity

            // process attacks
            let (attacks, world) = World.doSubscription "Attack" (Events.AttackEvent --> Simulants.GameplayScene --> Address.Wildcard) world
            let world =
                FQueue.fold (fun world (attacked : Entity) ->
                    let world = attacked.HitPoints.Map (dec >> max 0) world
                    if attacked.GetHitPoints world > 0 then
                        if not (attacked.GetActionState world).IsInjuryState then
                            let world = attacked.SetActionState (InjuryState { InjuryTime = world.ClockTime }) world
                            let world = attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                            world
                        else world
                    else
                        if not (attacked.GetActionState world).IsWoundState then
                            let world = attacked.SetActionState (WoundState { WoundTime = world.ClockTime; WoundEventPublished = false }) world
                            let world = attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                            world
                        else world)
                    world attacks

            // process enemy deaths
            let (deaths, world) = World.doSubscription "Die" (Events.DieEvent --> Simulants.GameplayScene --> Address.Wildcard) world
            let enemyDeaths = FQueue.filter (fun (death : Entity) -> death.GetIsEnemy world) deaths
            let world = FQueue.fold (fun world death -> World.destroyEntity death world) world enemyDeaths
        
            // process player death
            let playerDeaths = FQueue.filter (fun (death : Entity) -> death.GetIsPlayer world) deaths
            let world = if FQueue.notEmpty playerDeaths then gameplay.SetGameplayState Quit world else world

            // update sun to shine over player as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let mutable shadowViewInverse = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion (sun.GetRotation world)
            shadowViewInverse.Translation <- sun.GetPosition world
            let shadowView = shadowViewInverse.Inverted
            let shadowWidth = sun.GetLightCutoff world * 2.0f
            let shadowResolution = Viewport.getShadowTextureBufferResolution 0 world.GeometryViewport
            let shadowTexelSize = shadowWidth / single shadowResolution.X // assuming square, of course
            let position = Simulants.GameplayPlayer.GetPosition world
            let positionShadow = position.Transform shadowView + v3Up * 12.0f // position of player + offset in shadow space
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowViewInverse
            let world = sun.SetPosition position world

            // update eye to look at player
            let world =
                if world.Advancing then
                    let position = player.GetPosition world
                    let rotation = player.GetRotation world
                    let world = World.setEye3dCenter (position + v3Up * 1.6f - rotation.Forward * 1.1f + rotation.Right * 0.25f) world
                    let world = World.setEye3dRotation rotation world
                    world
                else world

            // process nav sync
            let world = if initializing then World.synchronizeNav3d gameplay world else world

            // end scene declaration
            let world = World.endGroup world

            // declare gui group
            let world = World.beginGroup "Gui" [] world
            let (clicked, world) = World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world
            let world = if clicked then gameplay.SetGameplayState Quit world else world
            World.endGroup world

        // otherwise, no processing
        else world

    // this is a semantic fix-up that allows the editor to avoid creating an unused group. This is specific to the
    // ImNui API that is needed to patch a little semantic hole inherent in the immediate-mode programming idiom.
    override this.CreateDefaultGroup (screen, world) = World.createGroup (Some "Gui") screen world