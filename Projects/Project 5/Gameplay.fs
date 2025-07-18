namespace MyGame
open System
open System.Collections.Generic
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
        member this.GetInventory world : Inventory = this.Get (nameof Screen.Inventory) world
        member this.SetInventory (value : Inventory) world = this.Set (nameof Screen.Inventory) value world
        member this.Inventory = lens (nameof Screen.Inventory) this this.GetInventory this.SetInventory
        member this.GetAdvents world : Advent Set = this.Get (nameof Screen.Advents) world
        member this.SetAdvents (value : Advent Set) world = this.Set (nameof Screen.Advents) value world
        member this.Advents = lens (nameof Screen.Advents) this this.GetAdvents this.SetAdvents
        member this.GetHuntedTimeOpt world : GameTime option = this.Get (nameof Screen.HuntedTimeOpt) world
        member this.SetHuntedTimeOpt (value : GameTime option) world = this.Set (nameof Screen.HuntedTimeOpt) value world
        member this.HuntedTimeOpt = lens (nameof Screen.HuntedTimeOpt) this this.GetHuntedTimeOpt this.SetHuntedTimeOpt
        member this.GetStalkerSpawnAllowed world : bool = this.Get (nameof Screen.StalkerSpawnAllowed) world
        member this.SetStalkerSpawnAllowed (value : bool) world = this.Set (nameof Screen.StalkerSpawnAllowed) value world
        member this.StalkerSpawnAllowed = lens (nameof Screen.StalkerSpawnAllowed) this this.GetStalkerSpawnAllowed this.SetStalkerSpawnAllowed
        member this.GetStalkerSpawnState world : StalkerSpawnState = this.Get (nameof Screen.StalkerSpawnState) world
        member this.SetStalkerSpawnState (value : StalkerSpawnState) world = this.Set (nameof Screen.StalkerSpawnState) value world
        member this.StalkerSpawnState = lens (nameof Screen.StalkerSpawnState) this this.GetStalkerSpawnState this.SetStalkerSpawnState
        member this.GetDanger world : single = this.Get (nameof Screen.Danger) world
        member this.SetDanger (value : single) world = this.Set (nameof Screen.Danger) value world
        member this.Danger = lens (nameof Screen.Danger) this this.GetDanger this.SetDanger

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    static let processPlayerHidingSpot (hidingSpot : Entity) (doorSpotCollisionOpt : Entity option) (player : Entity) world =
        match player.GetActionState world with
        | NormalState ->
            if World.doButton "Hide" [Entity.Text .= "Hide"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideEntering }) world
                match doorSpotCollisionOpt with
                | Some doorSpot -> doorSpot.SetDoorState (DoorClosing world.GameTime) world
                | None -> ()
        | HideState hide ->
            match hide.HidePhase with
            | HideWaiting ->
                if World.doButton "Emerge" [Entity.Text .= "Emerge"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideEmerging }) world
                    match doorSpotCollisionOpt with
                    | Some doorSpot -> doorSpot.SetDoorState (DoorOpening world.GameTime) world
                    | None -> ()
            | _ -> ()
        | _ -> ()

    static let processPlayerInsertionSpot (insertionSpot : Entity) (player : Entity) (screen : Screen) world =
        match player.GetActionState world with
        | NormalState ->
            let insertionKey =
                insertionSpot.GetInsertionKey world
            World.beginPanel "Inventory"
                [Entity.Position .= v3 0.0f 144.0f 0.0f
                 Entity.Size .= v3 460.0f 40.0f 0.0f
                 Entity.Color .= Color.White.WithA 0.25f
                 Entity.Layout .= Flow (FlowRightward, FlowUnlimited)
                 Entity.LayoutMargin .= v2Dup 4.0f] world
            for (itemType, itemCount) in (screen.GetInventory world).Pairs do
                let itemName = scstringMemo itemType
                let clicked =
                    World.doButton itemName
                        [Entity.UpImage @= asset Assets.Gameplay.PackageName (itemName + "Up")
                         Entity.DownImage @= asset Assets.Gameplay.PackageName (itemName + "Down")
                         Entity.Size .= v3 32.0f 32.0f 0.0f] world
                if clicked && itemType = insertionKey then
                    player.SetActionState (InsertionState { InsertionSpot = insertionSpot }) world
                    insertionSpot.SetInsertionPhase (InsertionStarted world.GameTime) world
                    screen.Inventory.Map (fun inv ->
                        match inv.TryGetValue insertionKey with
                        | (true, count) -> if count > 1 then Map.add insertionKey (dec count) inv else Map.remove insertionKey inv
                        | (false, _) -> Log.error "Unexpected match error."; inv)
                        world
            World.endPanel world
        | InsertionState insertion ->
            match insertionSpot.GetInsertionPhase world with
            | InsertionNotStarted ->
                player.SetActionState NormalState world
            | InsertionStarted _ ->
                match insertionSpot.GetInteractionResult world with
                | Description description ->
                    World.doText "InteractionResult" [Entity.Text @= description; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                | Narration narration ->
                    World.doText "InteractionResult" [Entity.Text @= narration; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                | Find itemType ->
                    let itemNameSpaced = (scstringMemo itemType).Spaced
                    World.doText "InteractionResult" [Entity.Text @= "Found " + itemNameSpaced; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                | FindNonUnique (itemType, _) ->
                    let itemNameSpaced = (scstringMemo itemType).Spaced
                    World.doText "InteractionResult" [Entity.Text @= "Found " + itemNameSpaced; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                | EndGame ->
                    World.doText "InteractionResult" [Entity.Text @= "And the story ends here..."; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                | Nothing ->
                    World.doText "InteractionResult" [Entity.Text @= "Nothing of interest here..."; Entity.Size .= v3 640.0f 32.0f 0.0f] world
            | InsertionFinished ->
                match insertionSpot.GetInteractionResult world with
                | Description _ ->
                    player.SetActionState NormalState world
                | Narration narration ->
                    screen.Advents.Map (Set.add (Narrated narration)) world
                    player.SetActionState NormalState world
                | Find itemType ->
                    screen.Inventory.Map (Map.add itemType 1) world
                    screen.Advents.Map (Set.add (Found itemType)) world
                    player.SetActionState NormalState world
                | FindNonUnique (itemType, advent) ->
                    screen.Inventory.Map (Map.add itemType 1) world
                    screen.Advents.Map (Set.add advent) world
                    player.SetActionState NormalState world
                | EndGame ->
                    screen.SetGameplayState Quit world
                | Nothing ->
                    player.SetActionState NormalState world                                    
        | _ -> ()

    static let processPlayerDoorSpot (doorSpot : Entity) (player : Entity) (_ : Screen) world =
        match player.GetActionState world with
        | NormalState ->
            match doorSpot.GetDoorState world with
            | DoorClosed ->
                if World.doButton "OpenDoor" [Entity.Text .= "Open"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    doorSpot.SetDoorState (DoorOpening world.GameTime) world
            | DoorOpened ->
                if  doorSpot.GetClosable world &&
                    World.doButton "CloseDoor" [Entity.Text .= "Close"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    doorSpot.SetDoorState (DoorClosing world.GameTime) world
            | DoorClosing _ | DoorOpening _ -> ()
        | _ -> ()

    static let processPlayerInvestigationSpot (investigationSpot : Entity) (player : Entity) (screen : Screen) world =
        match player.GetActionState world with
        | NormalState ->
            if World.doButton "Investigate" [Entity.Text .= "Investigate"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                investigationSpot.SetInvestigationPhase (InvestigationStarted world.GameTime) world
                player.SetActionState (InvestigationState { InvestigationSpot = investigationSpot }) world
        | InvestigationState investigation ->
            match investigation.InvestigationSpot.GetInvestigationPhase world with
            | InvestigationNotStarted ->
                player.SetActionState NormalState world
            | InvestigationStarted startTime ->
                let localTime = world.GameTime - startTime
                if  localTime < 8.0f &&
                    World.doButton "Abandon" [Entity.Text .= "Abandon"; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    investigationSpot.SetInvestigationPhase InvestigationNotStarted world
                else investigationSpot.SetInvestigationPhase (InvestigationFinished world.GameTime) world
            | InvestigationFinished startTime ->
                let localTime = world.GameTime - startTime
                if localTime < 2.0f then
                    match investigationSpot.GetInteractionResult world with
                    | Description description ->
                        World.doText "InteractionResult" [Entity.Text @= description; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                    | Narration narration ->
                        World.doText "InteractionResult" [Entity.Text @= narration; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                    | Find itemType ->
                        let itemNameSpaced = (scstringMemo itemType).Spaced
                        World.doText "InteractionResult" [Entity.Text @= "Found " + itemNameSpaced; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                    | FindNonUnique (itemType, _) ->
                        let itemNameSpaced = (scstringMemo itemType).Spaced
                        World.doText "InteractionResult" [Entity.Text @= "Found " + itemNameSpaced; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                    | EndGame ->
                        World.doText "InteractionResult" [Entity.Text @= "And the story ends here..."; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                    | Nothing ->
                        World.doText "InteractionResult" [Entity.Text @= "Nothing of interest here..."; Entity.Size .= v3 640.0f 32.0f 0.0f] world
                else
                    match investigationSpot.GetInteractionResult world with
                    | Description _ -> ()
                    | Narration narration -> screen.Advents.Map (Set.add (Narrated narration)) world
                    | Find itemType ->
                        screen.Inventory.Map (fun inv -> Map.add itemType 1 inv) world
                        screen.Advents.Map (Set.add (Found itemType)) world
                    | FindNonUnique (itemType, advent) ->
                        screen.Inventory.Map (fun inv -> Map.add itemType 1 inv) world
                        screen.Advents.Map (Set.add advent) world
                    | EndGame -> screen.SetGameplayState Quit world
                    | Nothing -> ()
                    player.SetActionState NormalState world
        | _ -> ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Inventory Inventory.initial
         define Screen.Advents Set.empty
         define Screen.HuntedTimeOpt None
         define Screen.StalkerSpawnAllowed true
         define Screen.StalkerSpawnState StalkerSpawnState.initial
         define Screen.Danger 0.0f]

    // here we define the behavior of our gameplay
    override this.Process (screenResults, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // initialize gameplay state
            let initializing = FQueue.contains Select screenResults
            if initializing then
                World.mapRenderer3dConfig (fun config -> { config with SsrEnabled = true }) world
                screen.SetStalkerSpawnState (StalkerUnspawned world.GameTime) world

            // begin scene declaration, processing nav sync at end of frame since optimized representations like frozen
            // entities won't have their nav info registered until then
            World.beginGroupFromFile "Scene" "Assets/ClassicMansion/ClassicMansion.nugroup" [] world
            if initializing then World.defer (World.synchronizeNav3d false (Some "Assets/ClassicMansion/ClassicMansion.nav") screen) screen world

            // collect spawn points
            let entitiesSovereign = World.getSovereignEntities Simulants.GameplayScene world                
            let spawnPoints =
                entitiesSovereign |>
                Seq.map (fun room -> room.GetChildren world |> Seq.filter (fun container -> container.Name = "SpawnPoints")) |>
                Seq.concat |>
                Seq.map (fun node -> node.GetChildren world |> Seq.filter (fun child -> child.Is<SpawnPointDispatcher> world)) |>
                Seq.concat |>
                Seq.toArray

            // collect characters
            let characters =
                entitiesSovereign |>
                Seq.map (fun room -> room.GetChildren world |> Seq.filter (fun container -> container.Name = "Enemies")) |>
                Seq.concat |>
                Seq.map (fun node -> node.GetChildren world |> Seq.filter (fun child -> child.Is<CharacterDispatcher> world)) |>
                Seq.concat |>
                Seq.append (entitiesSovereign |> Seq.filter (fun entity -> entity.Is<CharacterDispatcher> world)) |>
                Seq.toArray

            // declare player
            World.doEntity<PlayerDispatcher> "Player"
                [if initializing then
                    Entity.Position @= v3 5.5f -1.35f -2.5f
                    Entity.Degrees @= v3 0.0f 90.0f 0.0f
                 Entity.Size .= v3 1.5f 2.0f 1.5f
                 Entity.Offset .= v3 0.0f 1.0f 0.0f
                 Entity.AnimatedModel .= Assets.Gameplay.SophieModel] world
            let player = world.DeclaredEntity

            // process player interaction spots
            let hidingSpotCollisionOpt = player.GetHidingSpotCollisions world |> Seq.filter (fun c -> c.GetExists world && c.GetBodyEnabled world) |> Seq.tryHead
            let insertionSpotCollisionOpt = player.GetInsertionSpotCollisions world |> Seq.filter (fun c -> c.GetExists world && c.GetBodyEnabled world) |> Seq.tryHead
            let doorSpotCollisionOpt = player.GetDoorSpotCollisions world |> Seq.filter (fun c -> c.GetExists world && c.GetBodyEnabled world) |> Seq.tryHead
            let investigationSpotCollisionOpt = player.GetInvestigationSpotCollisions world |> Seq.filter (fun c -> c.GetExists world && c.GetBodyEnabled world) |> Seq.tryHead
            match hidingSpotCollisionOpt with
            | Some hidingSpot -> processPlayerHidingSpot hidingSpot doorSpotCollisionOpt player world
            | None ->
                match insertionSpotCollisionOpt with
                | Some insertionSpot -> processPlayerInsertionSpot insertionSpot player screen world
                | None ->
                    match doorSpotCollisionOpt with
                    | Some doorSpot -> processPlayerDoorSpot doorSpot player screen world
                    | None ->
                        match investigationSpotCollisionOpt with
                        | Some investigationSpot -> processPlayerInvestigationSpot investigationSpot player screen world
                        | None -> ()

            // declare inventory view
            match player.GetActionState world with
            | InventoryState ->
                World.beginPanel "Inventory"
                    [Entity.Position .= v3 0.0f 144.0f 0.0f
                     Entity.Size .= v3 460.0f 40.0f 0.0f
                     Entity.Color .= Color.White.WithA 0.25f
                     Entity.Layout .= Flow (FlowRightward, FlowUnlimited)
                     Entity.LayoutMargin .= v2Dup 4.0f] world
                for (itemType, itemCount) in (screen.GetInventory world).Pairs do
                    let itemName = scstringMemo itemType
                    World.doButton itemName
                        [Entity.UpImage @= asset Assets.Gameplay.PackageName (itemName + "Up")
                         Entity.DownImage @= asset Assets.Gameplay.PackageName (itemName + "Down")
                         Entity.Size .= v3 32.0f 32.0f 0.0f] world |> ignore<bool>
                World.endPanel world
                if World.doButton "Close Inventory" [Entity.Text .= "Close Inv."; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    player.SetActionState NormalState world
            | InvestigationState _ | InsertionState _ | WoundState _ -> ()
            | _ ->
                if  hidingSpotCollisionOpt.IsNone &&
                    insertionSpotCollisionOpt.IsNone &&
                    doorSpotCollisionOpt.IsNone &&
                    investigationSpotCollisionOpt.IsNone &&
                    World.doButton "Open Inventory" [Entity.Text .= "Open Inv."; Entity.Position .= v3 232.0f -104.0f 0.0f] world then
                    player.SetActionState InventoryState world

            // process stalker spawn state
            if screen.GetStalkerSpawnAllowed world then
                match screen.GetStalkerSpawnState world with
                | StalkerUnspawned unspawnTime ->
                    let unspawnDuration = world.GameTime - unspawnTime
                    if unspawnDuration >= Constants.Gameplay.StalkDelay && spawnPoints.Length > 0 then
                        let spawnPoint = Gen.randomItem spawnPoints
                        screen.SetStalkerSpawnState (StalkerStalking (false, spawnPoint, world.GameTime)) world
                | StalkerStalking (caughtTargetHiding, spawnPoint, spawnTime) ->
                    let spawnDuration = world.GameTime - spawnTime
                    if spawnDuration >= Constants.Gameplay.StalkDuration && not caughtTargetHiding then
                        screen.SetStalkerSpawnState (StalkerLeaving (spawnPoint, spawnTime)) world
                | StalkerLeaving (_, _) -> ()
            else
                match screen.GetStalkerSpawnState world with
                | StalkerStalking (_, spawnPoint, spawnTime) ->
                    screen.SetStalkerSpawnState (StalkerLeaving (spawnPoint, spawnTime)) world
                | StalkerUnspawned _ | StalkerLeaving (_, _) -> ()

            // declare stalker
            match screen.GetStalkerSpawnState world with
            | StalkerStalking (caughtTargetHiding, spawnPoint, spawnTime) ->

                // declare stalker in spawned state
                let spawnPosition = spawnPoint.GetPosition world
                let characterState =
                    let awareness =
                        if caughtTargetHiding
                        then AwareOfTargetHiding world.GameTime
                        else AwareOfTargetTraversing world.GameTime
                    StalkerState (StalkingState { SpawnPosition = spawnPosition; Awareness = awareness })
                World.doEntity<StalkerDispatcher> "Stalker"
                    [if spawnTime = world.GameTime then
                        Entity.Position @= spawnPosition
                        Entity.CharacterState @= characterState] world
                let stalker = world.DeclaredEntity

                // process potentially resetting to late spawn state
                let position = stalker.GetPosition world
                let rotation = stalker.GetRotation world
                let bodyId = stalker.GetBodyId world
                let playerEhs = player / Constants.Gameplay.CharacterExpandedHideSensorName
                let playerBodyIds = Set.ofList [player.GetBodyId world; playerEhs.GetBodyId world]                    
                if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId playerBodyIds world then
                    let caughtTargetHiding = match player.GetActionState world with HideState hide -> hide.HidePhase.IsHideEntering | _ -> false
                    let resetTime = max spawnTime (world.GameTime - (Constants.Gameplay.StalkDuration - GameTime.ofSeconds 10.0f))
                    screen.SetStalkerSpawnState (StalkerStalking (caughtTargetHiding, spawnPoint, resetTime)) world

            | StalkerLeaving (unspawnPoint, _) ->

                // declare stalker in unspawning state
                let unspawnPosition = unspawnPoint.GetPosition world
                let stalkerState = LeavingState { UnspawnPosition = unspawnPosition; Awareness = UnawareOfTarget }
                World.doEntity<StalkerDispatcher> "Stalker" [Entity.CharacterState @= StalkerState stalkerState] world
                let stalker = world.DeclaredEntity

                // process resetting to late spawn state or unspawning
                let position = stalker.GetPosition world
                let rotation = stalker.GetRotation world
                let bodyId = stalker.GetBodyId world
                let playerEhs = player / Constants.Gameplay.CharacterExpandedHideSensorName
                let playerBodyIds = Set.ofList [player.GetBodyId world; playerEhs.GetBodyId world]                    
                if Algorithm.getTargetInSight Constants.Gameplay.EnemySightDistance position rotation bodyId playerBodyIds world then
                    let caughtTargetHiding = match player.GetActionState world with HideState hide -> hide.HidePhase.IsHideEntering | _ -> false
                    let resetTime = world.GameTime - (Constants.Gameplay.StalkDuration - GameTime.ofSeconds 10.0f)
                    screen.SetStalkerSpawnState (StalkerStalking (caughtTargetHiding, unspawnPoint, resetTime)) world
                elif (stalker.GetPosition world).Distance unspawnPosition < 0.5f then
                    screen.SetStalkerSpawnState (StalkerUnspawned world.GameTime) world

            | StalkerUnspawned _ -> ()

            // process hunted time
            let hunted =
                characters |>
                Array.exists (fun character ->
                    match character.GetCharacterState world with
                    | HunterState state -> not state.HunterAwareness.IsUnawareOfTarget
                    | _ -> false)
            match (hunted, screen.GetHuntedTimeOpt world) with
            | (true, None) -> screen.SetHuntedTimeOpt (Some world.GameTime) world
            | (false, _) -> screen.SetHuntedTimeOpt None world
            | (_, _) -> ()

            // process song playback
            let ambientSong = Assets.AbandonedMansion.CountryNightCrickets
            let huntedDurationOpt = match screen.GetHuntedTimeOpt world with Some huntedTime -> Some (world.GameTime - huntedTime) | None -> None
            let stalkedDurationOpt = (screen.GetStalkerSpawnState world).SpawnDurationOpt world.GameTime
            match (huntedDurationOpt, stalkedDurationOpt) with
            | (_, Some _) ->
                match World.getSongOpt world with
                | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.StalkedSong ->
                    World.playSong 0.0f 0.0f 0.0f None 0.3f Assets.Gameplay.StalkedSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 0.3f Assets.Gameplay.StalkedSong world
                | Some _ -> ()
            | (Some _, _) ->
                let playHuntedSong =
                    match World.getSongOpt world with
                    | Some songDescriptor -> songDescriptor.Song <> Assets.Gameplay.HuntedSong || World.getSongFadingOut world
                    | None -> true
                if playHuntedSong then
                    World.playSong 0.0f 0.0f 0.0f None 0.3f Assets.Gameplay.HuntedSong world
            | (None, None) ->
                match World.getSongOpt world with
                | Some songDescriptor ->
                    if (songDescriptor.Song = Assets.Gameplay.HuntedSong || songDescriptor.Song = Assets.Gameplay.StalkedSong) && not (World.getSongFadingOut world) then
                        World.fadeOutSong 7.5f world
                    elif songDescriptor.Song <> ambientSong && not (World.getSongFadingOut world) then
                        World.playSong 0.0f 0.0f 0.0f None 1.0f ambientSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f ambientSong world

            // process danger
            screen.Danger.Map (fun danger ->
                match max huntedDurationOpt stalkedDurationOpt with
                | Some dangerDuration -> min 1.0f dangerDuration.Seconds
                | None -> max 0.0f (danger - world.GameDelta.Seconds / 7.5f))
                world

            // process lighting
            let fillLightColor = Color.Lerp (Color.White, Color.Red, screen.GetDanger world)
            Simulants.GameplayFillLight.SetColor fillLightColor world

            // process attacks
            for character in characters do
                for attacked in World.doSubscription "Attacks" character.AttackEvent world do
                    match attacked.GetActionState world with
                    | HideState hide when hide.HidePhase.IsHideUncovered -> attacked.SetHitPoints 0 world
                    | _ -> attacked.HitPoints.Map (dec >> max 0) world
                    let actionState = attacked.GetActionState world
                    if attacked.GetHitPoints world > 0 then
                        if not actionState.IsInjuryState then
                            match actionState with
                            | InvestigationState investigation -> investigation.InvestigationSpot.SetInvestigationPhase InvestigationNotStarted world
                            | _ -> ()
                            attacked.SetActionState (InjuryState { InjuryTime = world.GameTime }) world
                            attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                    elif not actionState.IsWoundState then
                        attacked.SetActionState (WoundState { WoundTime = world.GameTime; WoundEventPublished = false }) world
                        attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world

            // process deaths
            for character in characters do
                for dead in World.doSubscription "Deaths" character.DeathEvent world do
                    match dead.GetCharacterType world with
                    | Hunter | Stalker -> World.destroyEntity dead world
                    | Player -> screen.SetGameplayState Quit world

            // update sun to shine over player as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let mutable shadowViewInverse = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion (sun.GetRotation world)
            shadowViewInverse.Translation <- sun.GetPosition world
            let shadowView = shadowViewInverse.Inverted
            let shadowWidth = max (sun.GetLightCutoff world * 2.0f) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
            let shadowTexelSize = shadowWidth / single world.GeometryViewport.ShadowTextureResolution.X // assuming square shadow texture, of course
            let position = Simulants.GameplayPlayer.GetPositionInterpolated world
            let positionShadow = position.Transform shadowView
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowViewInverse
            sun.SetPositionLocal position world

            // update eye to look at player
            if world.Advancing then
                let actionState = player.GetActionState world
                let position = player.GetPositionInterpolated world
                let eyeRotation = Simulants.GameplayPlayer.GetRotationInterpolated world * Quaternion.CreateFromAxisAngle (v3Right, -0.25f)
                let eyeDistanceScalar = Algorithm.computeEyeDistanceScalar position eyeRotation actionState player world
                let eyeOrigin = position + v3Up * Constants.Gameplay.PlayerEyeLevel
                let eyeDistancing = eyeRotation.Forward * Constants.Gameplay.PlayerEyeDistance * eyeDistanceScalar
                let eyeFlipping = if (player.GetPlayerState world).ViewFlip then -1.0f else 1.0f
                let eyeShifting = eyeRotation.Right * Constants.Gameplay.PlayerEyeShift * eyeDistanceScalar * eyeFlipping
                let eyeCenter = eyeOrigin - eyeDistancing + eyeShifting
                World.setEye3dCenter eyeCenter world
                World.setEye3dRotation eyeRotation world

            // declare quit button
            if World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world then
                screen.SetGameplayState Quit world

            // end scene declaration
            World.endGroup world

        // otherwise, clear game state
        else
            screen.SetInventory Inventory.initial world
            screen.SetAdvents Set.empty world

    override this.Edit (op, _, world) =
        match op with
        | ViewportOverlay _ ->

            // show spawn points
            let entitiesInView = World.getEntities3dInView (HashSet ()) world
            for entity in entitiesInView do
                match entity.GetDispatcher world with
                | :? WayPointDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 10.0f false Color.Yellow world
                | :? SpawnPointDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 10.0f false Color.Magenta world
                | :? InsertionSpotDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 5.0f false Color.LightGreen world
                | :? DoorSpotDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 5.0f false Color.Brown world
                | :? InvestigationSpotDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 5.0f false Color.LightBlue world
                | :? HidingSpotDispatcher -> World.imGuiCircle3d (entity.GetPosition world) 5.0f false Color.Turquoise world
                | _ -> ()

        | _ -> ()