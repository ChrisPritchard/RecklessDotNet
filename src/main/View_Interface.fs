module Main.View_Interface

open Xelmish.Model
open Xelmish.Viewables
open Constants
open LayoutHelper
open Model

//let orderSelectTabRects, orderSelectButtonRects = 
//    let tabPanel, buttonPanel =
//        rowsAndCols [0.2;0.8] [] controlPanelRect
//        |> fun a -> a.[0], a.[1]
//    let tabRects = rowsAndCols [] [0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125] tabPanel
//    let buttonRects = 
//        rowsAndCols [0.25; 0.25; 0.25; 0.25] [0.25; 0.25; 0.25; 0.25] buttonPanel
//        |> Array.map (marginPad 5 0 >> fst) 
//    tabRects, buttonRects

let button displayText action enabled (x, y, w, h) =
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

        yield normalText (-0.5, -0.5) displayText textPos
        if enabled then 
            yield onclick action (w, h) (x, y) ]

let corpInfo corporation (x, y, w, h) = [
    let rects = 
        rowsAndCols [0.18;0.13;0.13;0.13;0.13;0.13] [0.5;0.5] (x, y, w, h)
        |> Array.map (contractBy defaultMargin)

    yield colour colours.background (w, h) (x, y)
    yield setSmoothSampling ()
    yield titleText (0., 0.) corporation.displayName (topLeft rects.[0])

    let listItem i (label, value) =
        [
            normalText (-1., 0.) label (topRight rects.[1 + i])
            normalText (0., 0.) value (topLeft rects.[8 + i])
        ]

    yield! [
        "Ideas", string corporation.ideas
        "Prospects", "TODO"
        "Stock Value", "TODO"
        "Market Share", "TODO"
        "Cash", sprintf "$%i" corporation.cash
        "Expenses", "TODO"
    ] |> List.mapi listItem |> List.collect id
]

let executiveInfo executive dispatch (x, y, w, h) = [
    let split = rowsAndCols [] [0.5;0.5] (x, y, w, h)
    let stats = 
        rowsAndCols [0.18;0.13;0.13;0.13;0.21;0.21] [] split.[0]
        |> Array.skip 1 |> Array.map (contractBy defaultMargin)

    yield colour colours.background (w, h) (x, y)

    yield normalText (0., 0.) (sprintf "LVL:   1") (topLeft stats.[0])
    yield normalText (0., 0.) (sprintf "XP:    0") (topLeft stats.[1])
    yield normalText (0., 0.) (sprintf "TODO") (topLeft stats.[2])
    yield! button "Orders" (fun _ -> ()) true stats.[3]
    yield! button "Corp Report" (fun _ -> ()) false stats.[4]

    let (px, py, pw, ph) = contractBy defaultMargin split.[1]
    yield colour colours.temp (pw, ph) (px, py)
]

let selectedInfo selected (x, y, w, h) = [
    yield colour colours.background (w, h) (x, y)
]

let informationPanels corporation selected dispatch windowRect =     
    let corpInfoRect, execRect, selectedInfoRect = 
        rowsAndCols [] [0.3; 0.3] windowRect
        |> fun a -> a.[0], a.[1], a.[2]
    [
        yield! corpInfo corporation corpInfoRect
        yield! executiveInfo corporation dispatch execRect
        yield! selectedInfo selected selectedInfoRect
    ]

let renderUserInterface model dispatch = 
    let controlPanelRect = 
        let percentage = 0.3
        rowsAndCols [1. - percentage; percentage] [] (0, 0, windowWidth, windowHeight)
        |> Array.last
    match model.currentInterface with
    | Information selectedTile -> 
        let selected = model.market.atTile selectedTile
        let corp = match selected with Some (OfficeInfo o) -> o.corporation | _ -> model.market.player
        informationPanels corp selected dispatch controlPanelRect
    | _ -> []