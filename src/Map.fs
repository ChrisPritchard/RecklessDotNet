module Reckless.Map

open Microsoft.Xna.Framework
open GameCore.GameModel
open Constants
open Turn
open Model
open Iso

let private renderMap (mx, my) gameState =
    let tilePopup = gameState.mouseTile |> Option.defaultValue (-1, -1)
    gameState.map
    |> Set.toList
    |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th

            match Map.tryFind (x, y) gameState.productTiles with
            | Some ((corp, _)::_) -> 
                yield 0, Image ("tile", rect, corp.colour)
            | _ -> 
                yield 0, Image ("tile", rect, Color.White)

            if (x, y) = (mx, my) || (x, y) = tilePopup then
                yield 2, Image ("tile-highlight", rect, Color.White)
        ])



let private renderOffice colour map mouseTile selectedTile office = 
    [
        let pos = office.x, office.y
        let rect = isoRect office.x office.y tw (th*3)
        yield 1, Image ("office", rect, colour)

        if pos = mouseTile || pos = selectedTile then
            yield 3, Image ("office-highlight", rect, Color.White)

        if pos = selectedTile then
            let tiles = localProductTiles office
            yield! tiles |> List.filter (fun p -> Set.contains p map) |> List.map (fun (x, y) ->
                let tileRect = isoRect x y tw th
                2, Image ("tile-highlight", tileRect, Color.White))
    ]

let private renderOffices mouseTile gameState =
    let selectedTile = gameState.mouseTile |> Option.defaultValue (-1, -1)
    gameState.corps 
    |> List.collect (fun corp -> 
        allOffices corp.headOffice 
        |> List.collect (renderOffice corp.colour gameState.map mouseTile selectedTile))

let render mouseTile gameState = [
    yield! renderMap mouseTile gameState
    yield! renderOffices mouseTile gameState
]