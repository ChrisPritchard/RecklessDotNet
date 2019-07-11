module Main.View_Interface

open Xelmish.Model
open Xelmish.Viewables
open Constants
open Model

/// Divides a rectangle into rows and columns, with rows and columns given as float percentages.
/// Ignores rows/columns that do not fix, and/or truncates them as necessary. 
/// Also returns remainder space as a rect if the outer rect is not totally consumed.
/// Returns a list of subrects in the same x,y,w,h tuple format as the outer rect.
let private rowsAndCols (rows: float list) (cols: float list) (x, y, w, h) =
    let folder (results, remainder) segment = 
        if remainder = 0 then results, remainder
        elif remainder < segment then remainder::results, 0
        else segment::results, remainder - segment
    
    let rowHeights = 
        Seq.map (fun rowDef -> int (rowDef * float h)) rows
        |> Seq.fold folder ([], h) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res
        |> Seq.rev

    let colWidths = 
        Seq.map (fun colDef -> int (colDef * float w)) cols
        |> Seq.fold folder ([], w) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res 
        |> Seq.rev
        |> Seq.toList // colWidths is iterated over repeatedly, so compute once

    let colFolder currentTop height currentLeft width =
        (currentLeft, currentTop, width, height), currentLeft + width
    let rowFolder currentTop height =
        Seq.mapFold (colFolder currentTop height) x colWidths |> fst, currentTop + height

    Seq.mapFold rowFolder y rowHeights 
    |> fst |> Seq.collect id 
    |> Seq.sortBy (fun (x, y, _, _) -> x, y) 
    |> Seq.toArray
    
let private topLeft (x, y, _, _) = x, y
let private topRight (x, y, w, _) = x + w, y
let private middle (x, y, w, h) = x + w / 2, y + h / 2

/// Returns a rectangle that is within the out rectangle by a margin. 
/// Also returns a rectangle inside this first rectangle by a padding amount.
let private marginPad margin padding (x, y, w, h) =
    (x + margin, y + margin, w - (margin * 2), h - (margin * 2)),
    (x + margin + padding, y + margin + padding, w - ((margin + padding) * 2), h - ((margin + padding) * 2))

let contains (tx, ty) (x, y, w, h) =
    tx >= x && tx < x + w &&
    ty >= y && ty < y + h

//let orderSelectTabRects, orderSelectButtonRects = 
//    let tabPanel, buttonPanel =
//        rowsAndCols [0.2;0.8] [] controlPanelRect
//        |> fun a -> a.[0], a.[1]
//    let tabRects = rowsAndCols [] [0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125; 0.125] tabPanel
//    let buttonRects = 
//        rowsAndCols [0.25; 0.25; 0.25; 0.25] [0.25; 0.25; 0.25; 0.25] buttonPanel
//        |> Array.map (marginPad 5 0 >> fst) 
//    tabRects, buttonRects
    
let colours = {|
        background = Colour.DarkGray
        text = Colour.White
        button = Colour.Blue
        buttonDisabled = Colour.Gray
        buttonHover = Colour.LightBlue
        buttonPressed = Colour.DarkBlue
    |}
let normalText = text "defaultFont" 20. colours.text
let titleText = text "defaultFont" 25. colours.text

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

        yield normalText (-0.5, -0.5) displayText textPos
        if enabled then 
            yield onclick action (w, h) (x, y) ]

let corpInfo corporation (x, y, w, h) = [
    let rects = 
        rowsAndCols [0.18;0.14;0.14;0.14;0.14;0.14;0.14] [0.5;0.5] (x, y, w, h)
        |> Array.map (marginPad 5 0 >> fst)

    yield colour colours.background (w, h) (x, y)
    yield setSmoothSampling ()
    yield titleText (0., 0.) corporation.name (topLeft rects.[0])

    let listItem i (label, value) =
        [
            normalText (-1., 0.) label (topRight rects.[2 + (i*2)])
            normalText (0., 0.) value (topLeft rects.[2 + (i*2) + 1])
        ]

    yield! [
        "Ideas", string corporation.ideas
        //"Prospects", "TODO"
        //"Stock Value", "TODO"
        //"Market Share", "TODO"
        //"Cash", sprintf "$%i" corporation.cash
        //"Expenses", "TODO"
    ] |> List.mapi listItem |> List.collect id
]
let executiveInfo corporation dispatch (x, y, w, h) = [
    yield colour colours.background (w, h) (x, y)
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