module Main.View_Interface

open Xelmish.Model
open Xelmish.Viewables
open Constants

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
    
/// Simple mapping function on a rect to return its position.
/// Use with rowsAndCols or similar if you don't care about width and height
let private topLeft (x, y, _, _) = x, y

/// Simple function to return the point at the middle of a rect.
/// Useful when placing centre-aligned text.
let private middle (x, y, w, h) = x + w / 2, y + h / 2

/// Takes a rect and splits it down the middle into left and right halves, returned as an array.
let private splitVertically (x, y, w, h) =
    [|(x, y, w/2, h); (x + w/2, y, w/2, h)|]

/// Takes a rect and splits it across the middle into top and bottom halves, returned as an array. 
let private splitHorizontally (x, y, w, h) =
    [|(x, y, w, h/2); (x, y + h/2, w, h/2)|]

/// Returns a rectangle that is within the out rectangle by a margin. 
/// Also returns a rectangle inside this first rectangle by a padding amount.
let private marginPad margin padding (x, y, w, h) =
    (x + margin, y + margin, w - (margin * 2), h - (margin * 2)),
    (x + margin + padding, y + margin + padding, w - ((margin + padding) * 2), h - ((margin + padding) * 2))

let contains (tx, ty) (x, y, w, h) =
    tx >= x && tx < x + w &&
    ty >= y && ty < y + h

let controlPanelRect = 
    let percentage = 0.3
    rowsAndCols [1. - percentage; percentage] [] (0, 0, windowWidth, windowHeight)
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


let renderUserInterface model dispatch = 
    // the user interface is in one of three modes:
    // - general info or default: shows the player's corp, their selected executive, and the selected entity's information
    // - order screen, giving order options
    // - order select screen, giving instructions on what to select (also shows the currently selected entity)
    [ ]