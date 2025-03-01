namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type SpawnPointDispatcher () =
    inherit Entity3dDispatcherImNui (false, false, false)

    override this.PresenceOverride =
        ValueSome Exterior

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero