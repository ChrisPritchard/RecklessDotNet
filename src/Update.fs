module Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model
open Constants
open StartModel
open Iso
open Interface

let findMouseTile runState gameState =
    if not (isMousePressed (true, false) runState) then
        gameState.selectedTile
    else 
        let tile = mouseTile runState
        if Set.contains tile gameState.market 
        then Some tile else None

let advanceModel runState (uiModel: UIModel) model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            match gameState.phase with
            | Orders when uiModel.endTurn -> 
                Some { gameState with phase = TurnEnding runState.elapsed }
            | TurnEnding startTime when runState.elapsed - startTime >= turnTransitionTime ->
                // TODO: advanceTurn
                Some { gameState with phase = TurnStarting runState.elapsed }
            | TurnStarting startTime when runState.elapsed - startTime >= turnTransitionTime ->
                Some { gameState with phase = Orders }
            | _ ->
                Some { gameState with selectedTile = findMouseTile runState gameState }