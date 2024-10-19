namespace MyGame
open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quitting
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
    inherit ScreenDispatcher ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Run (gameplay, world) =

        // declare scene group when selected
        let world =
            if gameplay.GetSelected world then

                // begin scene declaration
                let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

                // declare player
                let (_, world) =
                    World.doCharacter3d "Player"
                        [Entity.Position .= v3 1.0f 0.0f -1.0f
                         Entity.Size .= v3 1.5f 2.0f 1.5f
                         Entity.Offset .= v3 0.0f 1.0f 0.0f
                         Entity.AnimatedModel .= Assets.Gameplay.Sophie] world
                let player = world.RecentEntity
                let playerBodyId = player.GetBodyId world

                // move player
                let playerSpeed = 1.8f * world.GameDelta.Seconds
                let playerRotation = player.GetRotation world
                let playerVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.W world then playerRotation.Forward * playerSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world then playerRotation.Back * playerSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then playerRotation.Left * playerSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then playerRotation.Right * playerSpeed else v3Zero)
                let grounded = World.getBodyGrounded playerBodyId world
                let turnSpeed = 1.8f * world.GameDelta.Seconds * if grounded then 1.0f else 0.75f
                let turnVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f) +
                    (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f)
                let world = player.SetPosition (player.GetPosition world + playerVelocity) world
                let world = player.SetRotation (player.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)) world

                // update eye to look at player
                let world =
                    if world.Advancing then
                        let position = player.GetPosition world
                        let rotation = player.GetRotation world
                        let world = World.setEye3dCenter (position + v3Up * 1.8f - rotation.Forward * 1.0f + rotation.Right * 0.25f) world
                        let world = World.setEye3dRotation rotation world
                        world
                    else world

                // end scene declaration
                World.endGroup world
            else world

        // declare gui group
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world
        let world = if clicked then gameplay.SetGameplayState Quitting world else world
        let world = World.endGroup world
        world

    // this is a semantic fix-up that allows the editor to avoid creating an unused group. This is specific to the
    // ImNui API that is needed to patch a little semantic hole inherent in the immediate-mode programming idiom.
    override this.CreateDefaultGroup (screen, world) = World.createGroup (Some "Gui") screen world