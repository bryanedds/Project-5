﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System

/// A participant in the event graph.
type Simulant =
    interface
        abstract SimulantAddress : Simulant Address
        end

[<AutoOpen>]
module SimulantOperators =

    /// Operators for the Simulant type.
    type Simulant with

        /// The names of the simulant.
        member this.Names = Address.getNames this.SimulantAddress

        /// The name of the simulant.
        member this.Name = Address.getName this.SimulantAddress

        /// Concatenate an address with a simulant's address, taking the type of first address.
        static member acatf<'a> (address : 'a Address) (simulant : Simulant) =
            match box simulant with
            | null -> address // HACK: this case is a hack to be able to insert events into an MMCC event handler
            | _ -> acatf address simulant.SimulantAddress

        /// Concatenate an address with a simulant's address, taking the type of first address.
        // Disabled due to extension types not supporting operators: static member (-->) (address, simulant : Simulant) = Simulant.acatf address simulant

/// A simulant in the event system that is globalized and compatible with generalized events.
type GlobalSimulantGeneralized =
    { GsgAddress : GlobalSimulantGeneralized Address }
    interface Simulant with
        member this.SimulantAddress = atoa<GlobalSimulantGeneralized, Simulant> this.GsgAddress
        end