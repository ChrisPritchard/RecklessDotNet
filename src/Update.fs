module Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model
open StartModel
open Iso

let private findProductTiles gameState =
    allCorps gameState
    |> List.collect (fun c -> 
            allProductTiles None c.headOffice
            |> List.map (fun (x, y, q) -> (x, y), (c, q)))
    |> List.groupBy fst
    |> List.map (fun (pos, tiles) -> 
        let ordered = 
            tiles 
            |> List.sortByDescending (fun (_, (_, q)) -> q) 
            |> List.map snd
        pos, ordered)
    |> Map.ofList

let findMouseTile runState gameState =
    if not (isMousePressed (true, false) runState) then
        gameState.selectedTile
    else 
        let tile = mouseTile runState
        if Set.contains tile gameState.market 
        then Some tile else None

let advanceModel runState model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            Some { gameState with 
                    selectedTile = findMouseTile runState gameState
                    productTiles = findProductTiles gameState }