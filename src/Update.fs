module Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model
open StartModel
open Iso

let findMouseTile runState gameState =
    if not (isMousePressed (true, false) runState) then
        gameState.selectedTile
    else 
        let tile = mouseTile runState
        if Set.contains tile gameState.market 
        then Some tile else None

let advanceModel runState _ model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            Some { gameState with 
                    selectedTile = findMouseTile runState gameState }