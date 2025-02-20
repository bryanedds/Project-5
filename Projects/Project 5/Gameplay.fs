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
        member this.GetHuntedTimeOpt world : single option = this.Get (nameof Screen.HuntedTimeOpt) world
        member this.SetHuntedTimeOpt (value : single option) world = this.Set (nameof Screen.HuntedTimeOpt) value world
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
                then screen.SetStalkerSpawnState (StalkerUnspawned world.ClockTime) world
                else world

            // begin scene declaration
            let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

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
                    [if initializing then Entity.Position @= v3 1.0f 0.0f -1.0f
                     Entity.Size .= v3 1.5f 2.0f 1.5f
                     Entity.Offset .= v3 0.0f 1.0f 0.0f
                     Entity.AnimatedModel .= Assets.Gameplay.SophieModel] world
            let player = world.DeclaredEntity

            // process stalker spawn state
            let world =
                if screen.GetStalkerSpawnAllowed world then
                    match screen.GetStalkerSpawnState world with
                    | StalkerUnspawned unspawnTime as state ->
                        let unspawnDuration = world.ClockTime - unspawnTime
                        let state =
                            if unspawnDuration >= 180.0f && spawnPoints.Length > 0 then
                                let spawnPoint = Gen.randomItem spawnPoints
                                StalkerSpawned (spawnPoint, world.ClockTime)
                            else state
                        screen.SetStalkerSpawnState state world
                    | StalkerSpawned (spawnPoint, spawnTime) as state ->
                        let spawnDuration = world.ClockTime - spawnTime
                        let state =
                            if spawnDuration >= 120.0f
                            then StalkerUnspawning (spawnPoint, spawnTime)
                            else state
                        screen.SetStalkerSpawnState state world
                    | StalkerUnspawning _ ->
                        world
                else
                    match screen.GetStalkerSpawnState world with
                    | StalkerSpawned (spawnPoint, spawnTime) ->
                        let state = StalkerUnspawning (spawnPoint, spawnTime)
                        screen.SetStalkerSpawnState state world
                    | StalkerUnspawned _ | StalkerUnspawning _ ->
                        world

            // declare stalker
            let world =
                match screen.GetStalkerSpawnState world with
                | StalkerSpawned (spawnPoint, spawnTime) ->

                    // declare stalker in spawned state
                    World.doEntity<StalkerDispatcher> "Stalker"
                        [if spawnTime = world.ClockTime then Entity.Position @= spawnPoint.GetPosition world
                         Entity.CharacterState @= StalkerState Spawned]
                        world

                | StalkerUnspawning (unspawnPoint, _) ->

                    // declare stalked in unspawning state
                    let unspawnPosition = unspawnPoint.GetPosition world
                    let stalkerState = Unspawning unspawnPosition
                    let world =
                        World.doEntity<StalkerDispatcher> "Stalker"
                            [Entity.CharacterState @= StalkerState stalkerState]
                            world
                    let stalker = world.DeclaredEntity
                    
                    // process player sighting
                    let playerSightings =
                        seq {
                            let position = stalker.GetPosition world
                            let rotation = stalker.GetRotation world
                            for scanSegment in StalkerState.computeScanSegments position rotation stalkerState do
                                let intersected = World.rayCast3dBodies scanSegment Int32.MaxValue false world
                                if  intersected.Length > 1 &&
                                    intersected.[1].BodyShapeIntersected.BodyId.BodySource = Simulants.GameplayPlayer then
                                    true }

                    // process unspawn or resetting to late spawn state
                    if Seq.isEmpty playerSightings then
                        if (stalker.GetPosition world).Distance unspawnPosition < 0.5f
                        then screen.SetStalkerSpawnState (StalkerUnspawned world.ClockTime) world
                        else world
                    else screen.SetStalkerSpawnState (StalkerSpawned (unspawnPoint, world.ClockTime - 110.0f)) world

                | StalkerUnspawned _ -> world

            // process hunted time
            let world =
                let hunted =
                    characters |>
                    Array.exists (fun character ->
                        match character.GetCharacterState world with
                        | HunterState state -> (state.HunterAwareDurationOpt world.ClockTime).IsSome
                        | _ -> false)
                match (hunted, screen.GetHuntedTimeOpt world) with
                | (true, None) -> screen.SetHuntedTimeOpt (Some world.ClockTime) world
                | (false, _) -> screen.SetHuntedTimeOpt None world
                | (_, _) -> world

            // process song playback
            let huntedDurationOpt = match screen.GetHuntedTimeOpt world with Some huntedTime -> Some (world.ClockTime - huntedTime) | None -> None
            let stalkedDurationOpt = (screen.GetStalkerSpawnState world).SpawnDurationOpt world.ClockTime
            match (huntedDurationOpt, stalkedDurationOpt) with
            | (_, Some _) ->
                match World.getSongOpt world with
                | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.StalkedSong ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                | Some _ -> ()
            | (Some _, _) ->
                match World.getSongOpt world with
                | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.HuntedSong ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.HuntedSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.HuntedSong world
                | Some _ -> ()
            | (None, None) ->
                match World.getSongOpt world with
                | Some songDescriptor ->
                    if (songDescriptor.Song = Assets.Gameplay.HuntedSong || songDescriptor.Song = Assets.Gameplay.StalkedSong) && not (World.getSongFadingOut world) then
                        World.fadeOutSong 7.5f world
                    elif songDescriptor.Song <> Assets.Gameplay.StealthSong && not (World.getSongFadingOut world) then
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StealthSong world
                | None ->
                    World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StealthSong world

            // process danger
            let world =
                screen.Danger.Map (fun danger ->
                    match max huntedDurationOpt stalkedDurationOpt with
                    | Some dangerDuration -> min 1.0f dangerDuration
                    | None -> max 0.0f (danger - world.ClockDelta / 7.5f))
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
                    let world = attack.HitPoints.Map (dec >> max 0) world
                    if attack.GetHitPoints world > 0 then
                        if not (attack.GetActionState world).IsInjuryState then
                            let world = attack.SetActionState (InjuryState { InjuryTime = world.ClockTime }) world
                            let world = attack.SetLinearVelocity (v3Up * attack.GetLinearVelocity world) world
                            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                            world
                        else world
                    else
                        if not (attack.GetActionState world).IsWoundState then
                            let world = attack.SetActionState (WoundState { WoundTime = world.ClockTime; WoundEventPublished = false }) world
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
            world

        | _ -> world