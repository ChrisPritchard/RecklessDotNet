module Interface

//open GameCore.GameModel
//open Model
//open Constants
    
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