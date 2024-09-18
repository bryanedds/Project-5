// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

// NOTE: would probably need to become more of an Xtension-based type of thing.
type ButtonState =
    { mutable Transform : Transform
      mutable Pressed : bool }

type ImWorld () =

    static let mutable world = Unchecked.defaultof<World>

    static let context = Address.empty : obj Address

    static let buttonStates = Dictionary<obj Address, ButtonState> ()

    static member button (name : string, text, transform, ?disabledColor, ?sliceMargin, ?upImage, ?downImage, ?textColor, ?textDisabledColor, ?font, ?fontSizing, ?fontStyling, ?justification) =

        // defaults
        let disabledColor = defaultArg disabledColor Constants.Gui.DisabledColorDefault
        let sliceMargin = defaultArg sliceMargin Constants.Gui.SliceMarginDefault
        let upImage = defaultArg upImage Assets.Default.ButtonUp
        let downImage = defaultArg downImage Assets.Default.ButtonDown
        let textColor = defaultArg textColor Color.White
        let textDisabledColor = defaultArg textDisabledColor Constants.Gui.DisabledColorDefault
        let font = defaultArg font Assets.Default.Font
        let fontSizing = defaultArg fontSizing None
        let fontStyling = defaultArg fontStyling Set.empty
        let justification = defaultArg justification (Justified (JustifyCenter, JustifyMiddle))

        // result and existence
        let mutable result = false
        let buttonAddress = Address.makeFromArray (Array.add name context.Names)
        let buttonState =
            match buttonStates.TryGetValue buttonAddress with
            | (true, buttonState) -> buttonState
            | (false, _) ->
                let buttonState = { Transform = transform; Pressed = false }
                buttonStates.[buttonAddress] <- buttonState
                buttonState
        buttonState.Transform.Active <- true

        // up transition
        if  buttonState.Pressed &&
            MouseState.isButtonUp MouseLeft &&
            buttonState.Transform.Bounds2d.Contains (MouseState.getPosition ()).V3 = ContainmentType.Contains then
            buttonState.Pressed <- false
            result <- true

        // down transition
        if  not buttonState.Pressed &&
            MouseState.isButtonDown MouseLeft &&
            buttonState.Transform.Bounds2d.Contains (MouseState.getPosition ()).V3 = ContainmentType.Contains then
            buttonState.Pressed <- true

        // render sprite
        let spriteImage = if buttonState.Pressed then downImage else upImage
        let color = if buttonState.Transform.Enabled then Color.One else disabledColor
        World.renderGuiSpriteSliced buttonState.Transform.Absolute buttonState.Transform.Perimeter sliceMargin spriteImage buttonState.Transform.Offset buttonState.Transform.Elevation color world

        // render text
        // TODO: factor this out like World.renderGuiSpriteSliced.
        if not (String.IsNullOrWhiteSpace text) then
            let perimeter = buttonState.Transform.Perimeter // gui currently ignores rotation and scale
            let horizon = buttonState.Transform.Horizon
            let mutable textTransform = Transform.makeDefault ()
            let margin = v3Zero//(entity.GetTextMargin world).V3
            let offset = v3Zero//(entity.GetTextOffset world).V3
            let shift = 0.0f//entity.GetTextShift world
            textTransform.Position <- perimeter.Center + margin + offset
            textTransform.Size <- perimeter.Size - margin * 2.0f
            textTransform.Elevation <- buttonState.Transform.Elevation + shift
            textTransform.Absolute <- buttonState.Transform.Absolute
            World.enqueueLayeredOperation2d
                { Elevation = textTransform.Elevation
                  Horizon = horizon
                  AssetTag = font
                  RenderOperation2d =
                    RenderText
                        { Transform = textTransform
                          ClipOpt = ValueSome buttonState.Transform.Bounds2d.Box2
                          Text = text
                          Font = font
                          FontSizing = fontSizing
                          FontStyling = fontStyling
                          Color = if buttonState.Transform.Enabled then textColor else textDisabledColor
                          Justification = justification }}
                world

        // fin
        result

    static member update () =
        for struct (buttonAddress, buttonState) in Array.ofSeq buttonStates.Pairs' do
            if not buttonState.Transform.Active
            then buttonStates.Remove buttonAddress |> ignore<bool>
            else buttonState.Transform.Active <- false

    static member init world_ =
        world <- world_

type ImGaia () =

    let world = ()

    // basically just an enhanced big s-expression editor