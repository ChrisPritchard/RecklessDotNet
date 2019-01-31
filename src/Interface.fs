module Interface

open ImGuiNET
open GameCore.ImGui.Wrappers
open Model
open Constants
    
//let private tilePopup corpList =
//    let qualityLine (corp, quality) =
//        sprintf "%s: %i" corp.abbreviation quality
//    let head = List.head corpList |> qualityLine
//    let rest = match corpList with | _::rest -> rest |> List.map qualityLine | _ -> []
//    popup head rest

//let private officePopup office corp =
//    let corpName = sprintf "%s (%s)" corp.name corp.abbreviation
//    let departments = office.departments |> List.map (sprintf "%A")
//    popup corpName departments

//let private findOfficePopup (mx, my) gameState = 
//    let office = 
//        allCorps gameState
//        |> List.collect (fun c -> allOffices c.headOffice |> List.map (fun o -> o, c))
//        |> List.tryFind (fun (o, _) -> o.x = mx && o.y = my)
//    office |> Option.bind (fun (o, c) ->
//        Some (officePopup o c))

//let private findTilePopup (mx, my) productTiles = 
//    match Map.tryFind (mx, my) productTiles with
//    | Some corpList -> tilePopup corpList
//    | _ -> []

//let private playerStats productTiles gameState =
//    let expenses = Map.find gameState.player (expensesByCorp departmentCost gameState)
//    let income = Map.find gameState.player (incomeByCorp productTiles productIncome)
//    let stats = [
//        "Cash", "$", gameState.player.cash
//        "Income", "$", income
//        "Expenses", "$", expenses
//        "Change", (if income > expenses then "$+" else "$"), income - expenses
//        "Ideas", "", gameState.player.ideas
//        "Orders Remaining", "", 0
//    ]
//    let lines = stats |> List.map (fun o -> o |||> sprintf "%s: %s%i")

//    let x, y, width, height = 10, winh - 140, 200, 130

//    [ 
//        yield! panel (x, y, width, height)
//        yield! lines |> List.mapi (fun i line -> 
//            let y = y + padding + (i * lineHeight)
//            Text (font, line, (x + padding, y), fontSize, TopLeft, activeColours.text))
//    ]

//let renderInterface productTiles gameState = 
//    [
//        yield! playerStats productTiles gameState

//        match gameState.selectedTile with
//        | Some (mx, my) ->
//            let popup = 
//                findOfficePopup (mx, my) gameState 
//                |> Option.defaultValue (findTilePopup (mx, my) productTiles)
//            yield! popup
//        | _ -> ()

//        yield! getButtonView gameState.buttons.endTurn
//    ]

type UIModel = {
    endTurn: bool
}

let flags = 
    ImGuiWindowFlags.NoResize ||| ImGuiWindowFlags.NoMove ||| ImGuiWindowFlags.NoCollapse

let window label pos size (flags: ImGuiWindowFlags) children =
    fun model textures ->
        match pos with 
            | None -> () 
            | Some (x, y) -> ImGui.SetNextWindowPos (new System.Numerics.Vector2 (float32 x, float32 y))
        match size with
            | None -> ()
            | Some (w, h) -> ImGui.SetNextWindowSize (new System.Numerics.Vector2 (float32 w, float32 h))
        ImGui.Begin (label, flags) |> ignore
        let next = (model, children) ||> List.fold (fun last child -> child last textures)
        ImGui.End ()
        next

let getInterface (gameState: GameState) =
    { endTurn = false },
    [
        yield (fun uimodel _ ->
            ImGui.StyleColorsLight ()
            uimodel
        )

        let statsFlags = flags ||| ImGuiWindowFlags.NoScrollbar
        let title = sprintf "%s (%s)" gameState.player.name gameState.player.abbreviation
        yield window title (Some (10, winh - 145)) (Some (220, 135)) statsFlags [
            yield text (sprintf "Cash                $%i" gameState.player.cash)
            yield text (sprintf "Income              $%i" gameState.player.cash)
            yield text (sprintf "Expenses            $%i" gameState.player.cash)
            yield text (sprintf "  Change            $%i" gameState.player.cash)
            yield text (sprintf "Product Ideas        %i" gameState.player.ideas)
            yield text (sprintf "Orders Remaining     %i" (2 - gameState.player.orders.Length))
        ]

        let endTurnFlags = flags ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoBackground
        yield window "" (Some (winw - 100, winh - 50)) None endTurnFlags [
            button "END TURN" (fun uimodel state -> { uimodel with endTurn = state })
        ]
    ]