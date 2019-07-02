module Main.UI

open Xelmish.Model
open Xelmish.Viewables
open Common
open Layout

let controlPanelRect = 
    let percentage = 0.3
    rowsAndCols [1. - percentage; percentage] [] (0, 0, winw, winh)
    |> Array.last

let corpInfoRect, execRect, selectedInfoRect = 
    rowsAndCols [] [0.3; 0.3] controlPanelRect
    |> fun a -> a.[0], a.[1], a.[2]

let orderSelectTabRects, orderSelectButtonRects = 
    let tabPanel, buttonPanel =
        rowsAndCols [0.2;0.8] [] controlPanelRect
        |> fun a -> a.[0], a.[1]
    let tabRects = rowsAndCols [] [0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125] tabPanel
    let buttonRects = 
        rowsAndCols [0.25; 0.25; 0.25; 0.25] [0.25; 0.25; 0.25; 0.25] buttonPanel
        |> Array.map (marginPad 5 0 >> fst) 
    tabRects, buttonRects


let text = text "defaultFont" 20.
let colours = {|
        text = Colour.White
        button = Colour.Blue
        buttonDisabled = Colour.LightBlue
        //hover = Colour.AliceBlue
        //pressed = Colour.DarkBlue
    |}

let button (x, y, w, h) displayText action enabled =
    let textPos = middle (x, y, w, h)
    let backColour = if enabled then colours.button else colours.buttonDisabled
    [   yield colour backColour (w, h) (x, y)
        yield text colours.text (-0.5, -0.5) displayText textPos
        if enabled then yield onclick action (w, h) (x, y) ]