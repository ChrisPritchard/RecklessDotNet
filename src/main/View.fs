﻿module Main.View

open Xelmish.Model
open Xelmish.Viewables
open Xelmish.UI
open Constants
open Model

let renderUserInterface (model: MainModel) dispatch = 
    let body = 
        match model.playerAction with
        | Overview -> 
            Views.InfoPanels.contentFor model dispatch
        | OrderTypeSelect (_, activeCategory) ->
            Views.OrderList.contentFor model activeCategory dispatch
        | TargetOrder orderState ->
            Views.CurrentOrder.contentFor model orderState dispatch
        | _ -> []

    let style = [
        colour colours.text
        backgroundColour colours.background 
        buttonTextColour colours.text
        buttonDisabledTextColour colours.textDisabled
        buttonBackgroundColour colours.button
        buttonDisabledColour colours.buttonDisabled
        buttonHoverColour colours.buttonHover
        buttonPressedColour colours.buttonPressed
    ]

    let all = 
        col [] [
            row [ height (pct 0.7) ] []
            row style body
        ]

    renderUI false "defaultFont" (0, 0) (windowWidth, windowHeight) all

let view model dispatch =
    [   yield setSmoothSampling ()
        yield! Views.Market.renderMarket model dispatch
        yield! renderUserInterface model dispatch

        if quitOnEscape then
            yield onkeydown Keys.Escape exit ]