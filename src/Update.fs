module Update

open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model
open Constants
open StartModel
open Helpers
open Iso

let findMouseTile runState gameState =
    if not (isMousePressed (true, false) runState) then
        gameState.selectedTile
    else 
        let tile = mouseTile runState
        if Set.contains tile gameState.market 
        then Some tile else None

let rec updateQuality office researchOffices =
    let hasResearch = List.contains Research office.departments && not <| List.contains office researchOffices
    let qaCount = office.extensions |> List.sumBy (fun e -> match e with QA -> 1)
    let newDepartments = 
        office.departments
        |> List.map (function
        | Product q -> 
            let degraded = if hasResearch then q else q - 10
            let enhanced = degraded + (qaCount * 5)
            Product (max 10 enhanced)
        | d -> d)
    let newManaged = office.managedOffices |> List.map (fun o -> updateQuality o researchOffices)
    { office with departments = newDepartments; managedOffices = newManaged }

let advanceTurn gameState = 
    let productTiles = gameProductTiles gameState
    let incomeByCorp = incomeByCorp productTiles productIncome
    let expensesByCorp = expensesByCorp departmentCost gameState

    let updateCorp corp = 
        { corp with
                cash = corp.cash + Map.find corp incomeByCorp - Map.find corp expensesByCorp
                headOffice = updateQuality corp.headOffice [] }

    { gameState with 
            player = updateCorp gameState.player
            others = gameState.others |> List.map updateCorp }

let advanceModel runState (uiModel: UIModel) model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some (startModel ())
        | Some gameState -> 
            match gameState.phase with
            | Orders when uiModel.endTurn -> 
                Some { gameState with phase = ConfirmEndTurn }
            | ConfirmEndTurn when uiModel.cancelEndTurn ->
                Some { gameState with phase = Orders }
            | ConfirmEndTurn when uiModel.confirmOrders ->
                Some { gameState with phase = TurnEnding runState.elapsed }
            | ConfirmEndTurn -> 
                Some gameState
            | TurnEnding startTime when runState.elapsed - startTime >= turnTransitionTime ->
                let gameState = advanceTurn gameState
                Some { gameState with phase = TurnStarting runState.elapsed }
            | TurnStarting startTime when runState.elapsed - startTime >= turnTransitionTime ->
                Some { gameState with phase = Orders }
            | _ ->
                Some { gameState with selectedTile = findMouseTile runState gameState }