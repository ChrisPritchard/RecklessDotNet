module Main.View_Interface

open Xelmish.Model
open Xelmish.Viewables
open Constants
open Model


/// Divides a rectangle into rows and columns, with rows and columns given as float percentages.
/// Ignores rows/columns that do not fix, and/or truncates them as necessary. 
/// Also returns remainder space as a rect if the outer rect is not totally consumed.
/// Returns a list of subrects in the same x,y,w,h tuple format as the outer rect.
/// Note: returns rects col by col
let rowsAndCols (rows: float list) (cols: float list) (x, y, w, h) =
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
    
let topLeft (x, y, _, _) = x, y
let topRight (x, y, w, _) = x + w, y
let middle (x, y, w, h) = x + w / 2, y + h / 2
let bottomCentre (x, y, w, h) = x + w / 2, y + h

/// Returns a rectangle that is within the out rectangle by a margin. 
let contractBy margin (x, y, w, h) =
    x + margin, y + margin, w - (margin * 2), h - (margin * 2)

let contains (tx, ty) (x, y, w, h) =
    tx >= x && tx < x + w &&
    ty >= y && ty < y + h

/// Draw a coloured rect if the predicate is true for the current inputs
let conditionalColour colour (width, height) (x, y) predicate = 
    OnDraw (fun loadedAssets inputs spriteBatch -> 
        if predicate inputs then
            spriteBatch.Draw(loadedAssets.whiteTexture, rect x y width height, colour))

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