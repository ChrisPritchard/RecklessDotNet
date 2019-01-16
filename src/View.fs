module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Constants
open Iso

let tw, th = tileSize

let lineHeight = fontSize * 4/3

let textHeight lines = 
    (lines - 1) * lineHeight + fontSize

let tilePopup corpList (tx, ty) = [
    let x, y, _, _ = isoRect tx ty tw th
    let topCorp = List.head corpList |> fun ((c: Corporation), q) -> sprintf "%s:\t%i" c.abbreviation q
    let rest = List.tail corpList |> List.map (fun (c, q) -> sprintf "%s:\t%i" c.abbreviation q)
    let textHeight = List.length rest + 1
    let textWidth = (topCorp::rest) |> List.map Seq.length |> Seq.max |> fun len -> len * 4/5
    let width, height = textWidth + padding*2, textHeight + padding*2

    yield Colour ((x, y, width, height), activeColours.background)
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