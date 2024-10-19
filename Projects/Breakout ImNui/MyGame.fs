﻿namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this determines what state the game is in. To learn about ImNui in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImNui
type GameState =
    | Splash
    | Title
    | Credits
    | Gameplay

// this extends the Game API to expose GameState as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGameDispatcher () =
    inherit GameDispatcher ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Splash]

    // here we handle running the game
    override this.Run (myGame, world) =

        // declare splash screen
        let (results, world) = World.beginScreen Simulants.Splash.Name (myGame.GetGameState world = Splash) (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] world
        let world = if FQueue.contains Deselecting results then myGame.SetGameState Title world else world
        let world = World.endScreen world

        // declare title screen
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Title.Name (myGame.GetGameState world = Title) (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Play" [] world
        let world = if clicked then myGame.SetGameState Gameplay world else world
        let (clicked, world) = World.doButton "Credits" [] world
        let world = if clicked then myGame.SetGameState Credits world else world
        let (clicked, world) = World.doButton "Exit" [] world
        let world = if clicked && world.Unaccompanied then World.exit world else world
        let world = World.endGroup world
        let world = World.endScreen world

        // declare gameplay screen
        let (results, world) = World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (myGame.GetGameState world = Gameplay) (Dissolve (Constants.Dissolve.Default, None)) [] world
        let world =
            if FQueue.contains Select results then
                let world = Simulants.Gameplay.SetGameplayState Playing world
                let bricks =
                    Map.ofList
                        [for i in 0 .. dec 5 do
                            for j in 0 .. dec 6 do
                                (Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))]
                let world = Simulants.Gameplay.SetBricks bricks world
                let world = Simulants.Gameplay.SetScore 0 world
                let world = Simulants.Gameplay.SetLives 5 world
                world
            else world
        let world =
            if FQueue.contains Deselecting results then
                let world = Simulants.Gameplay.SetGameplayState Quit world
                let world = Simulants.Gameplay.SetBricks Map.empty world
                let world = Simulants.Gameplay.SetScore 0 world
                let world = Simulants.Gameplay.SetLives 0 world
                world
            else world
        let world =
            if Simulants.Gameplay.GetSelected world && Simulants.Gameplay.GetGameplayState world = Quitting
            then myGame.SetGameState Title world
            else world
        let world = World.endScreen world

        // declare credits screen
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Credits.Name (myGame.GetGameState world = Credits) (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] world
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Back" [] world
        let world = if clicked then myGame.SetGameState Title world else world
        let world = World.endGroup world
        let world = World.endScreen world

        // handle Alt+F4 when not in editor
        let world =
            if world.Unaccompanied && World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
            then World.exit world
            else world

        // fin
        world