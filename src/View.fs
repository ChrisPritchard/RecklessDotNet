module View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Constants
open Model
open Iso

let xna = function
    | Red -> Color.Red
    | Orange -> Color.Orange
    | Yellow -> Color.Yellow 
    | Green -> Color.Green 
    | Blue -> Color.Blue 
    | Purple -> Color.Purple

let private renderMarket productTiles gameState =
    gameState.market
    |> Set.toList
    |> List.collect (fun (x, y) -> [
        let rect = isoRect x y tw th

        match Map.tryFind (x, y) productTiles with
        | Some ((corp, _)::_) -> 
            yield Image ("tile", rect, xna corp.colour)
        | _ -> 
            yield Image ("tile", rect, Color.White)
    ])

let private renderOffices gameState =
    allCorps gameState
    |> List.collect (fun corp -> allOffices corp.headOffice |> List.map (fun o -> corp, o))
    |> List.sortBy (fun (_, office) -> office.y, -office.x)
    |> List.map (fun (corp, office) -> 
        let rect = isoRect office.x office.y tw (th*3)
        Image ("office", rect, xna corp.colour))

let private renderHighlight gameState (x, y) = [
        yield Image ("tile-highlight", isoRect x y tw th, Color.White)

        let allOffices = allCorps gameState |> List.collect (fun corp -> allOffices corp.headOffice)
        match List.tryFind (fun office -> (office.x, office.y) = (x, y)) allOffices with
        | None -> ()
        | Some office ->
            let rect = isoRect office.x office.y tw (th*3)
            yield Image ("office-highlight", rect, Color.White)

            let tiles = officeProductTiles office
            yield! tiles |> List.filter (fun p -> Set.contains p gameState.market) |> List.map (fun (x, y) ->
                Image ("tile-highlight", isoRect x y tw th, Color.White))
    ]

let private renderCursor runState =
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))

let getView runState gameState = 
    let productTiles = gameProductTiles gameState
    let mousePos = mouseTile runState
    [
        yield! renderMarket productTiles gameState
        yield! renderOffices gameState

        if gameState.selectedTile <> None then
            yield! renderHighlight gameState (Option.get gameState.selectedTile)

        if gameState.market.Contains mousePos then
            yield! renderHighlight gameState mousePos

        yield! Interface.renderInterface productTiles gameState

        yield renderCursor runState
    ]