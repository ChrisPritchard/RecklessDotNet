module Main.View

open Xelmish.Model
open Xelmish.Viewables
open Xelmish.UI
open Constants
open Model

let renderUserInterface (model: MainModel) dispatch = 
    let body = 
        match model.currentInterface with
        | Information selectedTile -> 
            Views.InfoPanels.contentFor model selectedTile dispatch
        | OrderTypeSelect ->
            Views.OrderList.contentFor model dispatch
        | _ -> []

    let style = [
        colour colours.text
        backgroundColour colours.background 
        buttonTextColour colours.text
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

    renderUI true "defaultFont" (0, 0) (windowWidth, windowHeight) all

let view model dispatch =
    [   yield setSmoothSampling ()
        yield! Views.Market.renderMarket model dispatch
        yield! renderUserInterface model dispatch

        if quitOnEscape then
            yield onkeydown Keys.Escape exit ]