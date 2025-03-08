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
        member this.GetInventoryViewOpt world : InventoryView option = this.Get (nameof Screen.InventoryViewOpt) world
        member this.SetInventoryViewOpt (value : InventoryView option) world = this.Set (nameof Screen.InventoryViewOpt) value world
        member this.InventoryViewOpt = lens (nameof Screen.InventoryViewOpt) this this.GetInventoryViewOpt this.SetInventoryViewOpt
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
    inherit ScreenDispatcherImNui ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Inventory Inventory.initial
         define Screen.InventoryViewOpt None
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
            let world =
                if initializing
                then screen.SetStalkerSpawnState (StalkerUnspawned world.GameTime) world
                else world

            // begin scene declaration
            let world = World.beginGroupFromFile "Scene" "Assets/AbandonedMansion/AbandonedMansion.nugroup" [] world

            // collect spawn points
            let entitiesSovereign = World.getEntitiesSovereign Simulants.GameplayScene world                
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
            let world =
                World.doEntity<PlayerDispatcher> "Player"
                    [if initializing then Entity.Position @= v3 6.6f 11.6f 4.3f
                     Entity.Size .= v3 1.5f 2.0f 1.5f
                     Entity.Offset .= v3 0.0f 1.0f 0.0f
                     Entity.AnimatedModel .= Assets.Gameplay.SophieModel] world
            let player = world.DeclaredEntity

            // declare player pause button
            let world =
                if world.Advancing then
                    let (clicked, world) = World.doButton "Pause" [Entity.Text .= "Pause"; Entity.Position .= v3 232.0f -104.0f 0.0f] world
                    if clicked then World.setAdvancing false world else world
                else
                    let (clicked, world) = World.doButton "Unpause" [Entity.Text .= "Unpause"; Entity.Position .= v3 232.0f -104.0f 0.0f] world
                    if clicked then World.setAdvancing true world else world

            // declare inventory view button
            let world =
                match screen.GetInventoryViewOpt world with
                | Some inventoryView ->
                    let world =
                        Map.foldi (fun i world itemType itemCount ->
                            let itemName = scstringMemo itemType
                            let (_, world) =
                                World.doButton itemName
                                    [Entity.UpImage @= asset Assets.Gameplay.PackageName (itemName + "Up")
                                     Entity.DownImage @= asset Assets.Gameplay.PackageName (itemName + "Down")
                                     Entity.Position @= v3 (-232.0f + single i * 40.0f) 144.0f 0.0f
                                     Entity.Size .= v3 32.0f 32.0f 0.0f]
                                    world
                            world)
                            world (screen.GetInventory world).Items
                    let (clicked, world) = World.doButton "Close Inventory" [Entity.Text .= "Close Inv."; Entity.Position .= v3 232.0f -64.0f 0.0f] world
                    if clicked then screen.SetInventoryViewOpt None world else world
                | None ->
                    let (clicked, world) = World.doButton "Open Inventory" [Entity.Text .= "Open Inv."; Entity.Position .= v3 232.0f -64.0f 0.0f] world
                    if clicked then screen.SetInventoryViewOpt (Some { ItemSelectedOpt = None }) world else world

            // declare player interaction button
            let doorCollisionOpt = player.GetDoorCollisions world |> Seq.filter (fun c -> c.GetExists world) |> Seq.tryHead
            let investigationCollisionOpt = player.GetInvestigationCollisions world |> Seq.filter (fun c -> c.GetExists world) |> Seq.tryHead
            let hidingSpotCollisionOpt = player.GetHidingSpotCollisions world |> Seq.filter (fun c -> c.GetExists world) |> Seq.tryHead
            let world =
                match hidingSpotCollisionOpt with
                | Some hidingSpot ->
                    match player.GetActionState world with
                    | NormalState ->
                        let (clicked, world) = World.doButton "Hide" [Entity.Text .= "Hide"; Entity.Position .= v3 -232.0f -144.0f 0.0f] world
                        if clicked then
                            let world = player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideEntering }) world
                            match doorCollisionOpt with
                            | Some door -> door.SetDoorState (DoorClosing world.GameTime) world
                            | None -> world
                        else world
                    | HideState hide ->
                        match hide.HidePhase with
                        | HideWaiting ->
                            let (clicked, world) = World.doButton "Emerge" [Entity.Text .= "Emerge"; Entity.Position .= v3 -232.0f -144.0f 0.0f] world
                            if clicked then
                                let world = player.SetActionState (HideState { HideTime = world.GameTime; HidePhase = HideEmerging }) world
                                match doorCollisionOpt with
                                | Some door -> door.SetDoorState (DoorOpening world.GameTime) world
                                | None -> world
                            else world
                        | _ -> world
                    | _ -> world
                | None ->
                    match doorCollisionOpt with
                    | Some door ->
                        match player.GetActionState world with
                        | NormalState ->
                            match door.GetDoorState world with
                            | DoorClosed ->
                                let (clicked, world) = World.doButton "OpenDoor" [Entity.Text .= "Open"; Entity.Position .= v3 -232.0f -144.0f 0.0f] world
                                if clicked
                                then door.SetDoorState (DoorOpening world.GameTime) world
                                else world
                            | DoorOpened ->
                                if door.GetClosable world then
                                    let (clicked, world) = World.doButton "CloseDoor" [Entity.Text .= "Close"; Entity.Position .= v3 -232.0f -144f 0.0f] world
                                    if clicked
                                    then door.SetDoorState (DoorClosing world.GameTime) world
                                    else world
                                else world
                            | DoorClosing _ | DoorOpening _ -> world
                        | _ -> world
                    | None ->
                        match investigationCollisionOpt with
                        | Some investigation ->
                            match player.GetActionState world with
                            | NormalState ->
                                let (clicked, world) = World.doButton "Investigate" [Entity.Text .= "Investigate"; Entity.Position .= v3 -232.0f -144.0f 0.0f] world
                                if clicked then
                                    let world = investigation.SetInvestigationPhase (InvestigationStarted world.GameTime) world
                                    let world = player.SetActionState (InvestigateState { Investigation = investigation }) world
                                    world
                                else world
                            | InvestigateState state ->
                                match state.Investigation.GetInvestigationPhase world with
                                | InvestigationNotStarted ->
                                    player.SetActionState NormalState world
                                | InvestigationStarted startTime ->
                                    let localTime = world.GameTime - startTime
                                    if localTime < 8.0f then
                                        let (clicked, world) = World.doButton "Abandon" [Entity.Text .= "Abandon"; Entity.Position .= v3 -232.0f -144.0f 0.0f] world
                                        if clicked
                                        then investigation.SetInvestigationPhase InvestigationNotStarted world
                                        else world
                                    else investigation.SetInvestigationPhase (InvestigationFinished world.GameTime) world
                                | InvestigationFinished startTime ->
                                    let localTime = world.GameTime - startTime
                                    if localTime >= 2.0f
                                    then player.SetActionState NormalState world
                                    else world
                            | _ -> world
                        | None -> world

            // process stalker spawn state
            let world =
                if screen.GetStalkerSpawnAllowed world then
                    match screen.GetStalkerSpawnState world with
                    | StalkerUnspawned unspawnTime as state ->
                        let unspawnDuration = world.GameTime - unspawnTime
                        let state =
                            if unspawnDuration >= Constants.Gameplay.StalkDelay && spawnPoints.Length > 0 then
                                let spawnPoint = Gen.randomItem spawnPoints
                                StalkerStalking (false, spawnPoint, world.GameTime)
                            else state
                        screen.SetStalkerSpawnState state world
                    | StalkerStalking (caughtTargetHiding, spawnPoint, spawnTime) as state ->
                        let spawnDuration = world.GameTime - spawnTime
                        let state =
                            if spawnDuration >= Constants.Gameplay.StalkDuration && not caughtTargetHiding
                            then StalkerLeaving (spawnPoint, spawnTime)
                            else state
                        screen.SetStalkerSpawnState state world
                    | StalkerLeaving (_, _) -> world
                else
                    match screen.GetStalkerSpawnState world with
                    | StalkerStalking (_, spawnPoint, spawnTime) ->
                        let state = StalkerLeaving (spawnPoint, spawnTime)
                        screen.SetStalkerSpawnState state world
                    | StalkerUnspawned _ | StalkerLeaving (_, _) -> world

            // declare stalker
            let world =
                match screen.GetStalkerSpawnState world with
                | StalkerStalking (caughtTargetHiding, spawnPoint, spawnTime) ->

                    // declare stalker in spawned state
                    let spawnPosition = spawnPoint.GetPosition world
                    let world =
                        World.doEntity<StalkerDispatcher> "Stalker"
                            [if spawnTime = world.GameTime then
                                Entity.Position @= spawnPosition
                                Entity.CharacterState @=
                                    let awareness =
                                        if caughtTargetHiding
                                        then AwareOfTargetHiding world.GameTime
                                        else AwareOfTargetTraversing world.GameTime
                                    StalkerState (StalkingState { SpawnPosition = spawnPosition; Awareness = awareness })]
                            world
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
                    else world

                | StalkerLeaving (unspawnPoint, _) ->

                    // declare stalker in unspawning state
                    let unspawnPosition = unspawnPoint.GetPosition world
                    let stalkerState = LeavingState { UnspawnPosition = unspawnPosition; Awareness = UnawareOfTarget }
                    let world = World.doEntity<StalkerDispatcher> "Stalker" [Entity.CharacterState @= StalkerState stalkerState] world
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
                    else
                        if (stalker.GetPosition world).Distance unspawnPosition < 0.5f
                        then screen.SetStalkerSpawnState (StalkerUnspawned world.GameTime) world
                        else world

                | StalkerUnspawned _ -> world

            // process hunted time
            let world =
                let hunted =
                    characters |>
                    Array.exists (fun character ->
                        match character.GetCharacterState world with
                        | HunterState state -> not state.HunterAwareness.IsUnawareOfTarget
                        | _ -> false)
                match (hunted, screen.GetHuntedTimeOpt world) with
                | (true, None) -> screen.SetHuntedTimeOpt (Some world.GameTime) world
                | (false, _) -> screen.SetHuntedTimeOpt None world
                | (_, _) -> world

            // process song playback
            let ambientSong = Assets.AbandonedMansion.CountryNightCrickets
            let huntedDurationOpt = match screen.GetHuntedTimeOpt world with Some huntedTime -> Some (world.GameTime - huntedTime) | None -> None
            let stalkedDurationOpt = (screen.GetStalkerSpawnState world).SpawnDurationOpt world.GameTime
            match (huntedDurationOpt, stalkedDurationOpt) with
            | (_, Some _) ->
                match World.getSongOpt world with
                | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.StalkedSong ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                | Some _ -> ()
            | (Some _, _) ->
                let playHuntedSong =
                    match World.getSongOpt world with
                    | Some songDescriptor -> songDescriptor.Song <> Assets.Gameplay.HuntedSong || World.getSongFadingOut world
                    | None -> true
                if playHuntedSong then
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.HuntedSong world
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
            let world =
                screen.Danger.Map (fun danger ->
                    match max huntedDurationOpt stalkedDurationOpt with
                    | Some dangerDuration -> min 1.0f dangerDuration.Seconds
                    | None -> max 0.0f (danger - world.GameDelta.Seconds / 7.5f))
                    world

            // process lighting
            let world =
                let sunColor = Color.Lerp (Color.White, Color.Red, screen.GetDanger world)
                Simulants.GameplaySun.SetColor sunColor world

            // collect attacks
            let (attacks, world) =
                Seq.fold (fun (attacks, world) (character : Entity) ->
                    let (attacks', world) = World.doSubscription "Attacks" character.AttackEvent world
                    (FQueue.append attacks attacks', world))
                    (FQueue.empty, world)
                    characters

            // process attacks
            let world =
                FQueue.fold (fun world (attack : Entity) ->
                    let world =
                        match attack.GetActionState world with
                        | HideState hide when hide.HidePhase.IsHideUncovered -> attack.SetHitPoints 0 world
                        | _ -> attack.HitPoints.Map (dec >> max 0) world
                    let actionState = attack.GetActionState world
                    if attack.GetHitPoints world > 0 then
                        if not actionState.IsInjuryState then
                            let world =
                                match actionState with
                                | InvestigateState investigateState -> investigateState.Investigation.SetInvestigationPhase InvestigationNotStarted world
                                | _ -> world
                            let world = attack.SetActionState (InjuryState { InjuryTime = world.GameTime }) world
                            let world = attack.SetLinearVelocity (v3Up * attack.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                            world
                        else world
                    else
                        if not actionState.IsWoundState then
                            let world = attack.SetActionState (WoundState { WoundTime = world.GameTime; WoundEventPublished = false }) world
                            let world = attack.SetLinearVelocity (v3Up * attack.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                            world
                        else world)
                    world attacks

            // collect deaths
            let (deaths, world) =
                Seq.fold (fun (attacks, world) (character : Entity) ->
                    let (attacks', world) = World.doSubscription "Deaths" character.DeathEvent world
                    (FQueue.append attacks attacks', world))
                    (FQueue.empty, world)
                    characters

            // process deaths
            let world =
                FQueue.fold (fun world (death : Entity) ->
                    if (death.GetCharacterType world).IsEnemy
                    then World.destroyEntity death world
                    else screen.SetGameplayState Quit world)
                    world deaths

            // update sun to shine over player as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let mutable shadowViewInverse = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion (sun.GetRotation world)
            shadowViewInverse.Translation <- sun.GetPosition world
            let shadowView = shadowViewInverse.Inverted
            let shadowWidth = max (sun.GetLightCutoff world * 2.0f) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
            let shadowResolution = Viewport.getShadowTextureBufferResolution 0 world.GeometryViewport
            let shadowTexelSize = shadowWidth / single shadowResolution.X // assuming square, of course
            let position = Simulants.GameplayPlayer.GetPositionInterpolated world
            let positionShadow = position.Transform shadowView + v3Up * 4.0f // position of player + offset in shadow space
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowViewInverse
            let world = sun.SetPositionLocal position world

            // update eye to look at player
            let world =
                if world.Advancing then
                    let actionState = player.GetActionState world
                    let position = player.GetPositionInterpolated world
                    let rotation = Simulants.GameplayPlayer.GetRotationInterpolated world * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
                    let eyeDistanceScalar = Algorithm.computeEyeDistanceScalar position rotation actionState player world
                    let world = World.setEye3dCenter (position + v3Up * Constants.Gameplay.PlayerEyeLevel - rotation.Forward * 1.5333f * eyeDistanceScalar + rotation.Right * 0.25f * eyeDistanceScalar) world
                    let world = World.setEye3dRotation rotation world
                    world
                else world

            // process nav sync
            let world = if initializing then World.synchronizeNav3d screen world else world

            // end scene declaration
            let world = World.endGroup world

            // declare gui group
            let world = World.beginGroup "Gui" [] world
            let (clicked, world) = World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world
            let world = if clicked then screen.SetGameplayState Quit world else world
            World.endGroup world

        // otherwise, no processing
        else world

    override this.Edit (op, _, world) =
        match op with
        | ViewportOverlay _ ->

            // show spawn points
            let entitiesInView = World.getEntities3dInView (HashSet ()) world            
            let spawnPoints = entitiesInView |> Seq.filter (fun entity -> entity.Is<SpawnPointDispatcher> world)
            for spawnPoint in spawnPoints do World.imGuiCircle3d (spawnPoint.GetPosition world) 10.0f false Color.Magenta world

            // show way points
            let wayPoints = entitiesInView |> Seq.filter (fun entity -> entity.Is<WayPointDispatcher> world)
            for wayPoint in wayPoints do World.imGuiCircle3d (wayPoint.GetPosition world) 10.0f false Color.Yellow world

            // show investigations
            let wayPoints = entitiesInView |> Seq.filter (fun entity -> entity.Is<InvestigationDispatcher> world)
            for wayPoint in wayPoints do World.imGuiCircle3d (wayPoint.GetPosition world) 5.0f false Color.Green world
            world

        | _ -> world