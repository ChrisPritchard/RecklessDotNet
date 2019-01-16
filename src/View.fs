module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Constants
open Iso

let tw, th = tileSize

let lineHeight = float fontSize * 1.25 |> int
let textHeight lines = ((lines - 1) * lineHeight) + fontSize

let contract n (rx, ry, rw, rh) =
    rx + n, ry + n, rw - 2*n, rh - 2*n

let panel rect = [
    match activeColours.border with
    | Some (n, colour) ->
        yield Colour (rect, colour)
        let inner = contract n rect
        yield Colour (inner, activeColours.background)
    | _ ->
        yield Colour (rect, activeColours.background)
]
        

let tilePopup corpList (tx, ty) = [
    let qualityLine (corp, quality) =
        sprintf "%s: %i" corp.abbreviation quality

    let lines = corpList |> List.map qualityLine
    let textHeight = textHeight (List.length lines)
    let textWidth = float (lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    let x, y, _, _ = isoRect tx ty tw th
    let width, height = textWidth + padding*2, textHeight + padding*2

    yield! panel (x, y, width, height)
    yield! lines |> List.mapi (fun i line -> 
        let colour = if i = 0 then activeColours.text else inactiveColours.text
        let y = y + padding + (i * lineHeight)
        Text (font, line, (x + padding, y), fontSize, TopLeft, colour))
]

let private renderMap productTiles (mx, my) map =
    map
    |> Set.toList
    |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th
            match Map.tryFind (x, y) productTiles with
            | Some ((c, _)::_) -> yield 0, Image ("tile", rect, c.colour)
            | _ -> yield 0, Image ("tile", rect, Color.White)
            if (x, y) = (mx, my) then
                yield 2, Image ("tile-highlight", rect, Color.White)
        ])

let rec private allOffices office = [
    yield office
    yield! List.collect allOffices office.managedOffices
]

let private renderOffices (mx, my) corps =
    corps
    |> List.collect (fun corp -> 
        allOffices corp.headOffice
        |> List.collect (fun o -> [
            let rect = isoRect o.x o.y tw (th*3)
            yield 1, Image ("office", rect, corp.colour)
            if (o.x, o.y) = (mx, my) then
                yield 3, Image ("office-highlight", rect, Color.White)
        ]))

let private getCursor runState =
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))

let getView runState gameState = 
    [
        // render map and corps
        // render popups (office info, tile info)
        // render ui commands (orders, end turn)
        // render status (cash, cashflow, ideas)
        // render mouse cursor

        let mousePos = mouseTile runState |> Option.defaultValue (-1, -1)
        yield! renderMap gameState.productTiles mousePos gameState.map
        yield! renderOffices mousePos gameState.corps
        yield! gameState.ui |> List.map (fun va -> 90, va)
        yield 99, getCursor runState
    ] 
    |> List.sortBy fst 
    |> List.map snd 