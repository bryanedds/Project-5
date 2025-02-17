namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type SpawnPointDispatcher () =
    inherit Entity3dDispatcherImNui (false, false, false)

    static member Properties =
        [define Entity.Size (v3Dup 0.5f)]