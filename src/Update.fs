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

let advanceTurn gameState = 
    let productTiles = gameProductTiles gameState
    let incomeByCorp = incomeByCorp productTiles productIncome
    let expensesByCorp = expensesByCorp departmentCost gameState

    let updateCorp corp = 
        {
            corp with
                cash = corp.cash + Map.find corp incomeByCorp - Map.find corp expensesByCorp
                headOffice = updateQuality corp.headOffice []
        }

    { 
        gameState with 
            player = updateCorp gameState.player
            others = gameState.others |> List.map updateCorp
    }

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
                let gameState = advanceTurn gameState
                Some { gameState with phase = TurnStarting runState.elapsed }
            | TurnStarting startTime when runState.elapsed - startTime >= turnTransitionTime ->
                Some { gameState with phase = Orders }
            | _ ->
                Some { gameState with selectedTile = findMouseTile runState gameState }