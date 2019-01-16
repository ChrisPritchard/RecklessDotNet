module Reckless.Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model
open StartModel
open Iso
open Turn

let private findProductTiles corps =
    corps
    |> List.collect (fun c -> 
            productTiles None c.headOffice
            |> List.map (fun (x, y, q) -> (x, y), (c, q)))
    |> List.groupBy fst
    |> List.map (fun (pos, tiles) -> 
        let ordered = 
            tiles 
            |> List.sortByDescending (fun (_, (_, q)) -> q) 
            |> List.map snd
        pos, ordered)
    |> Map.ofList

let updateUI runState gameState =
    if not (isMousePressed (true, false) runState) then
        gameState.ui
    else 
        match mouseTile runState with
        | None -> []
        | Some (mx, my) ->
            match Map.tryFind (mx, my) gameState.productTiles with
            | None -> []
            | Some corpList ->
                View.tilePopup corpList (mx, my)

let advanceModel runState model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            let newUI = updateUI runState gameState
            let newProductTiles = findProductTiles gameState.corps
            Some { gameState with ui = newUI; productTiles = newProductTiles }