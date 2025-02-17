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
        member this.GetStalkerSpawnState world : StalkerSpawnState = this.Get (nameof Screen.StalkerSpawnState) world
        member this.SetStalkerSpawnState (value : StalkerSpawnState) world = this.Set (nameof Screen.StalkerSpawnState) value world
        member this.StalkerSpawnState = lens (nameof Screen.StalkerSpawnState) this this.GetStalkerSpawnState this.SetStalkerSpawnState
        member this.GetStalkerSpawnAllowed world : bool = this.Get (nameof Screen.StalkerSpawnAllowed) world
        member this.SetStalkerSpawnAllowed (value : bool) world = this.Set (nameof Screen.StalkerSpawnAllowed) value world
        member this.StalkerSpawnAllowed = lens (nameof Screen.StalkerSpawnAllowed) this this.GetStalkerSpawnAllowed this.SetStalkerSpawnAllowed

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImNui ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.StalkerSpawnState StalkerSpawnState.initial
         define Screen.StalkerSpawnAllowed true]

    // here we define the behavior of our gameplay
    override this.Process (screenResults, gameplay, world) =

        // only process when selected
        if gameplay.GetSelected world then

            // begin scene declaration
            let initializing = FQueue.contains Select screenResults
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
                if gameplay.GetStalkerSpawnAllowed world then
                    match gameplay.GetStalkerSpawnState world with
                    | StalkerUnspawned countDown ->
                        let countDown = countDown - world.ClockDelta
                        let state =
                            if countDown <= 0.0f && spawnPoints.Length > 0 then
                                let spawnPoint = Gen.randomItem spawnPoints
                                StalkerSpawned (spawnPoint, 60.0f + Gen.randomf1 60.0f)
                            else StalkerUnspawned countDown
                        gameplay.SetStalkerSpawnState state world
                    | StalkerSpawned (spawnPoint, countDown) ->
                        let countDown = countDown - world.ClockDelta
                        let state =
                            if countDown <= 0.0f
                            then StalkerUnspawning spawnPoint
                            else StalkerSpawned (spawnPoint, countDown)
                        gameplay.SetStalkerSpawnState state world
                    | StalkerUnspawning _ ->
                        world
                else
                    match gameplay.GetStalkerSpawnState world with
                    | StalkerSpawned (spawnPoint, _) ->
                        let state = StalkerUnspawning spawnPoint
                        gameplay.SetStalkerSpawnState state world
                    | StalkerUnspawned _ | StalkerUnspawning _ ->
                        world

            // declare stalker
            let world =
                match gameplay.GetStalkerSpawnState world with
                | StalkerSpawned (spawnPoint, _) ->
                    World.doEntity<StalkerDispatcher> "Stalker" [Entity.Position .= spawnPoint.GetPosition world] world
                | _ -> world

            // determine scenario state
            let hunted =
                characters |>
                Array.exists (fun character ->
                    match character.GetCharacterState world with
                    | HunterState state -> state.HunterAwareOfPlayerOpt.IsSome
                    | _ -> false)
            let stalked =
                characters |>
                Array.exists (fun character ->
                    match character.GetCharacterState world with
                    | StalkerState _ -> true
                    | _ -> false)
            let world =
                if stalked then
                    match World.getSongOpt world with
                    | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.StalkedSong ->
                        let world = Simulants.GameplaySun.SetColor Color.Red world
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                        world
                    | None ->
                        let world = Simulants.GameplaySun.SetColor Color.Red world
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StalkedSong world
                        world
                    | Some _ -> world
                elif hunted then
                    match World.getSongOpt world with
                    | Some songDescriptor when songDescriptor.Song <> Assets.Gameplay.HuntedSong ->
                        let world = Simulants.GameplaySun.SetColor Color.Red world
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.HuntedSong world
                        world
                    | None ->
                        let world = Simulants.GameplaySun.SetColor Color.Red world
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.HuntedSong world
                        world
                    | Some _ -> world
                else
                    match World.getSongOpt world with
                    | Some songDescriptor ->
                        if  (songDescriptor.Song = Assets.Gameplay.HuntedSong ||
                             songDescriptor.Song = Assets.Gameplay.StalkedSong) &&
                            not (World.getSongFadingOut world) then
                            World.fadeOutSong 7.0f world
                            world
                        elif songDescriptor.Song <> Assets.Gameplay.StealthSong && not (World.getSongFadingOut world) then
                            World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StealthSong world
                            Simulants.GameplaySun.SetColor Color.White world
                        else world
                    | None ->
                        World.playSong 0.0f 0.0f 0.0f None 1.0f Assets.Gameplay.StealthSong world
                        Simulants.GameplaySun.SetColor Color.White world

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
                    else gameplay.SetGameplayState Quit world)
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