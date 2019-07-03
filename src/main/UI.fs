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
        buttonDisabled = Colour.Gray
        buttonHover = Colour.LightBlue
        buttonPressed = Colour.DarkBlue
    |}

let button (x, y, w, h) displayText action enabled =
    let textPos = middle (x, y, w, h)
    let hovering inputs = contains (inputs.mouseState.X, inputs.mouseState.Y) (x, y, w, h)
    let notHovering = hovering >> not

    [   yield conditionalColour colours.buttonDisabled (w, h) (x, y) (fun _ -> 
            not enabled)
        yield conditionalColour colours.button (w, h) (x, y) (fun inputs -> 
            enabled && notHovering inputs)
        yield conditionalColour colours.buttonHover (w, h) (x, y) (fun inputs -> 
            enabled && hovering inputs)
        yield conditionalColour colours.buttonPressed (w, h) (x, y) (fun inputs -> 
            enabled && hovering inputs && inputs.mouseState.LeftButton = ButtonState.Pressed)

        yield text colours.text (-0.5, -0.5) displayText textPos
        if enabled then 
            yield onclick action (w, h) (x, y) ]