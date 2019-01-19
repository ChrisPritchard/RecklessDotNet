module Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open GameCore.UIElements
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

let updateButtonStates runState gameButtons = 
    { gameButtons with
        endTurn = Button.updateButton runState gameButtons.endTurn }

let advanceModel runState model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            Some { gameState with 
                    selectedTile = findMouseTile runState gameState
                    buttons = updateButtonStates runState gameState.buttons }