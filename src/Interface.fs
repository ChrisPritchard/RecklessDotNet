﻿module Interface

open System.Numerics
open ImGuiNET
open GameCore.ImGui.Wrappers
open Model
open Constants
    
type UIModel = {
    endTurn: bool
}

let flags = 
    ImGuiWindowFlags.NoResize ||| ImGuiWindowFlags.NoMove ||| ImGuiWindowFlags.NoCollapse

let window label pos size (flags: ImGuiWindowFlags) children =
    fun model textures ->
        match pos with 
            | None -> () 
            | Some (x, y) -> ImGui.SetNextWindowPos (new Vector2 (float32 x, float32 y))
        match size with
            | None -> ()
            | Some (w, h) -> ImGui.SetNextWindowSize (new Vector2 (float32 w, float32 h))
        ImGui.Begin (label, flags) |> ignore
        let next = (model, children) ||> List.fold (fun last child -> child last textures)
        ImGui.End ()
        next

let playerStats productTiles gameState = 
    let income = Map.find gameState.player (incomeByCorp productTiles productIncome)
    let expenses = Map.find gameState.player (expensesByCorp departmentCost gameState)

    let statsFlags = flags ||| ImGuiWindowFlags.NoScrollbar ||| ImGuiWindowFlags.NoInputs
    let title = sprintf "%s (%s)" gameState.player.name gameState.player.abbreviation
    window title (Some (10, winh - 145)) (Some (220, 135)) statsFlags [
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
        window corpName (Some (10, 10)) (Some (150, 150)) popupFlags [
            yield! List.map (fun d -> text (sprintf "%A" d)) office.departments
        ]
    | None -> 
        match Map.tryFind (mx, my) productTiles with
        | Some corpList -> 
            let qualityLine (corp, quality) =
                sprintf "%s: %i" corp.abbreviation quality
            window "" (Some (10, 10)) None (popupFlags ||| ImGuiWindowFlags.NoTitleBar) [
                yield! List.map (fun c -> text (qualityLine c)) corpList
            ]
        | _ -> fun m _ -> m

let getInterface (gameState: GameState) =
    { endTurn = false },
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

        let endTurnFlags = flags ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.AlwaysAutoResize
        yield window "" (Some (winw - 110, winh - 60)) None endTurnFlags [
            (fun uimodel _ -> 
                let res = ImGui.Button ("END TURN", new Vector2 (90.f, 40.f))
                { uimodel with endTurn = res })
        ]
    ]