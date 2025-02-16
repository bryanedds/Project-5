namespace MyGame
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DeathEvent = stoa<Entity> "Death/Event"