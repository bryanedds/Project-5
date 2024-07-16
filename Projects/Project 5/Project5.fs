namespace Project5
open System
open System.Numerics
open Prime
open Nu

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type Project5 =
    | Splash
    | Title
    | Credits
    | Gameplay

// this is our top-level MMCC message type.
type Project5Message =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    interface Message

// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type Project5Command =
    | Exit
    interface Command

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module Project5Extensions =
    type Game with
        member this.GetProject5 world = this.GetModelGeneric<Project5> world
        member this.SetProject5 value world = this.SetModelGeneric<Project5> value world
        member this.Project5 = this.ModelGeneric<Project5> ()

// this is the dispatcher that customizes the top-level behavior of our game. In here, we create screens as content and
// bind them up with events and properties.
type Project5Dispatcher () =
    inherit GameDispatcher<Project5, Project5Message, Project5Command> (Splash)

    // here we define the game's properties and event handling
    override this.Definitions (project5, _) =
        [Game.DesiredScreen :=
            match project5 with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
            if project5 = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
            Simulants.TitleCredits.ClickEvent => ShowCredits
            Simulants.TitlePlay.ClickEvent => ShowGameplay
            Simulants.TitleExit.ClickEvent => Exit
            Simulants.CreditsBack.ClickEvent => ShowTitle
            Simulants.Gameplay.QuitEvent => ShowTitle]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just Gameplay

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    // here we describe the content of the game, including all of its screens.
    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]