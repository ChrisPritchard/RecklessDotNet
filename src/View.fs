module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Iso
open Turn

let tw, th = Constants.tileSize

let private findProductTiles corps =
    corps
    |> List.collect (fun c -> 
            productTiles None c.headOffice
            |> List.map (fun (x, y, q) -> (x, y), (c, q)))
    |> List.groupBy fst
    |> List.map (fun (pos, tiles) -> 
        let best = tiles |> List.sortByDescending (fun (_, (_, q)) -> q) |> List.head |> snd
        pos, best)
    |> Map.ofList

let private renderMap productTiles (mx, my) map =
    map
    |> Set.toList
    |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th
            match Map.tryFind (x, y) productTiles with
            | Some (c, _) -> yield 0, Image ("tile", rect, c.colour)
            | None -> yield 0, Image ("tile", rect, Color.White)
            if (x, y) = (mx, my) then
                yield 2, Image ("tile-highlight", rect, Color.White)
        ])

let rec allOffices office = [
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

let getCursor runState =
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))

let getView runState (map, (corps: Corporation list), ui) = 
    [
        // render map and corps
        // render popups (office info, tile info)
        // render ui commands (orders, end turn)
        // render status (cash, cashflow, ideas)
        // render mouse cursor

        let mousePos = mouseTile runState |> Option.defaultValue (-1, -1)
        let productTiles = findProductTiles corps
        yield! renderMap productTiles mousePos map
        yield! renderOffices mousePos corps
        yield 99, getCursor runState
    ] 
    |> List.sortBy fst 
    |> List.map snd 