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

    yield normalText (0., 0.) (sprintf "LVL:   %i" executive.level) (topLeft stats.[0])
    yield normalText (0., 0.) (sprintf "XP:    %i" executive.experience) (topLeft stats.[1])
    yield normalText (0., 0.) executive.lastName (topLeft stats.[2])
    yield! button "Orders" (fun _ -> ()) true stats.[3]
    yield! button "Corp Report" (fun _ -> ()) false stats.[4]

    let (px, py, pw, ph) = contractBy defaultMargin split.[1]
    yield colour colours.temp (pw, ph) (px, py)
]

let departmentLabels departments = 
    let departmentLabel department =
        let cost = departmentCost department
        match department with
        | Marketing -> sprintf  "Marketing      -%i" cost
        | Research -> sprintf   "Research       -%i" cost
        | Admin _ -> sprintf    "Admin          -%i" cost
        | _ -> ""
    let products, other = departments |> List.partition (function Product _ -> true | _ -> false)
    let otherLabels = other |> List.map departmentLabel
    match products with
    | [] -> otherLabels
    | _ -> (sprintf "%i Products" (List.length products))::otherLabels

let selectedInfo selected (x, y, w, h) = [
    let split = rowsAndCols [] [0.3] (x, y, w, h)
    let infoRects = rowsAndCols [0.18;0.13;0.1;0.1;0.1;0.1;0.1;0.1] [] split.[1] |> Array.map topLeft
    
    yield colour colours.background (w, h) (x, y)
    let dx, dy, dw, dh = contractBy defaultMargin split.[0]
    let tx, ty = dx + ((dw - tileWidth) / 2), dy + (dh - (tileHeight*2))

    match selected with
    | Some (TileInfo ti) -> 
        match ti with 
        | (dominant, quality)::_ ->
            yield image "tile" dominant.colour (tileWidth, tileHeight) (tx, ty)

            yield titleText (0., 0.) dominant.displayName infoRects.[0]
            yield normalText (0., 0.) (sprintf "Quality        %i" quality) infoRects.[1]

            yield! ti |> List.skip 1 |> List.truncate 5 
                |> List.mapi (fun i (corp, _) ->
                    normalText (0., 0.) corp.displayName infoRects.[i + 2])
        | _ -> 
            yield image "tile" Colour.White (tileWidth, tileHeight) (tx, ty)
            yield titleText (0., 0.) "Empty Market Tile" infoRects.[0]
    | Some (OfficeInfo oi) ->
        let ox, oy = dx + ((dw - tileWidth) / 2), dy + (dh - (tileHeight*4))
        yield image "tile" oi.corporation.colour (tileWidth, tileHeight) (tx, ty)
        yield! View_Market.renderOffice ox oy tileWidth (tileHeight * 3) oi.corporation.colour oi.headOffice

        yield normalText (0., 0.) (sprintf "Cash Flow: %i     Q: %i" 0 oi.quality) infoRects.[1]
        yield! oi.office.departments
            |> departmentLabels
            |> List.mapi (fun i label ->
                normalText (0., 0.) label infoRects.[i + 2])
    | _ -> ()    
]

let informationPanels corporation selected dispatch windowRect =     
    let corpInfoRect, execRect, selectedInfoRect = 
        rowsAndCols [] [0.3; 0.3] windowRect
        |> fun a -> a.[0], a.[1], a.[2]
    [
        yield! corpInfo corporation corpInfoRect
        let executive = 
            match selected with
            | Some (OfficeInfo o) -> o.executive.Value
            | _ -> corporation.ceo
        yield! executiveInfo executive dispatch execRect
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
        let corp = 
            match selected with 
            | Some (OfficeInfo o) -> o.corporation 
            | Some (TileInfo ((dominant, _)::_)) -> dominant
            | _ -> model.market.player
        informationPanels corp selected dispatch controlPanelRect
    | _ -> []