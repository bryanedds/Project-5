namespace MyGame
open System
open Nu

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DamageEvent = stoa<Damage> "Damage/Event"
    let DeathEvent = stoa<unit> "Death/Event"