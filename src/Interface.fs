module Interface

open System
open System.Numerics
open ImGuiNET
open GameCore.ImGui.Wrappers
open Model
open Constants
open Helpers

let flags = 
    ImGuiWindowFlags.NoResize ||| ImGuiWindowFlags.NoMove ||| ImGuiWindowFlags.NoCollapse

let window label (x, y) (w, h) (flags: ImGuiWindowFlags) children =
    fun model textures ->
        ImGui.SetNextWindowPos (new Vector2 (float32 x, float32 y))
        ImGui.SetNextWindowSize (new Vector2 (float32 w, float32 h))
        ImGui.Begin (label, flags) |> ignore
        let next = (model, children) ||> List.fold (fun last child -> child last textures)
        ImGui.End ()
        next

let playerStats productTiles gameState = 
    let income = Map.find gameState.player (incomeByCorp productTiles productIncome)
    let expenses = expensesForCorp departmentCost gameState.player

    let statsFlags = flags ||| ImGuiWindowFlags.NoScrollbar ||| ImGuiWindowFlags.NoInputs ||| ImGuiWindowFlags.NoTitleBar
    window "player-info" (10, winh - 145) (220, 135) statsFlags [
        text (sprintf "Cash                $%i" gameState.player.cash)
        text (sprintf "Income              $%i" income)
        text (sprintf "Expenses            $%i" expenses)
        text (sprintf "  Change            $%s%i" (if income > expenses then "+" else "") (income - expenses))
        text (sprintf "Product Ideas        %i" gameState.player.ideas)
        text (sprintf "Orders Remaining     %i" (2 - gameState.player.orders.Length))
    ]

let popupFlags = 
    ImGuiWindowFlags.NoResize ||| ImGuiWindowFlags.NoMove 
    ||| ImGuiWindowFlags.NoCollapse ||| ImGuiWindowFlags.AlwaysAutoResize
    ||| ImGuiWindowFlags.NoScrollbar ||| ImGuiWindowFlags.NoInputs
    ||| ImGuiWindowFlags.NoFocusOnAppearing

let showPopup (mx, my) productTiles gameState = 
    allCorps gameState
    |> List.collect (fun c -> allOffices c.headOffice |> List.map (fun o -> o, c))
    |> List.tryFind (fun (o, _) -> o.x = mx && o.y = my)
    |> function
    | Some (office, corp) ->
        let corpName = sprintf "%s (%s)" corp.name corp.abbreviation
        let departments = List.map (fun d -> text (sprintf "%A" d)) office.departments
        window corpName (10, 10) (150, 150) popupFlags departments
    | None -> 
        match Map.tryFind (mx, my) productTiles with
        | Some corpList -> 
            let qualityLine (corp, quality) =
                sprintf "%s: %i" corp.abbreviation quality
            let corps = List.map (fun c -> text (qualityLine c)) corpList
            window "tile-info" (10, 10) (100, 150) (popupFlags ||| ImGuiWindowFlags.NoTitleBar) corps
        | _ -> fun m _ -> m

let endTurnButton: UIModel -> (string -> IntPtr) -> UIModel = 
    let endTurnFlags = 
        flags ||| ImGuiWindowFlags.NoTitleBar 
        ||| ImGuiWindowFlags.NoBackground 
        ||| ImGuiWindowFlags.AlwaysAutoResize
        ||| ImGuiWindowFlags.NoScrollbar
    window "end-turn" (winw - 110, winh - 60) (110, 60) endTurnFlags [
        (fun uimodel _ -> 
            let res = ImGui.Button ("END TURN", new Vector2 (90.f, 40.f))
            { uimodel with endTurn = res })
    ]

let turnOrders _ =
    let flags = flags ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoScrollbar
    let margin = 200
    window "confirm-orders" (margin, margin) (winw - margin*2, winh - margin*2) flags [
        (fun uimodel _ -> 
            let res = ImGui.Button ("Accept", new Vector2 (90.f, 40.f))
            { uimodel with confirmOrders = res })
        (fun uimodel _ -> 
            let res = ImGui.Button ("Cancel", new Vector2 (90.f, 40.f))
            { uimodel with cancelEndTurn = res })
    ]

let startUIModel = {
    endTurn = false
    confirmOrders = false
    cancelEndTurn = false
}

let getInterface (gameState: GameState) =
    startUIModel,
    match gameState.phase with
    | Orders ->
        [
            yield (fun uimodel _ ->
                ImGui.StyleColorsLight ()
                uimodel
            )
        
            let productTiles = gameProductTiles gameState

            yield playerStats productTiles gameState

            match gameState.selectedTile with
            | Some point -> yield showPopup point productTiles gameState
            | None -> ()

            yield endTurnButton
        ]
    | ConfirmEndTurn ->
        [turnOrders gameState]
    | _ -> []