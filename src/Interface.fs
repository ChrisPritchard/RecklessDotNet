module Interface

open GameCore.GameModel
open Model
open Constants
open GameCore.UIElements.Button

let private lineHeight = float fontSize * 1.3 |> int

let private contract n (rx, ry, rw, rh) =
    rx + n, ry + n, rw - 2*n, rh - 2*n

let private panel rect = [
    match activeColours.border with
    | Some (n, colour) ->
        yield Colour (rect, colour)
        let inner = contract n rect
        yield Colour (inner, activeColours.background)
    | _ ->
        yield Colour (rect, activeColours.background)
]

let popup heading lines =
    let textHeight = List.length lines * lineHeight + (float lineHeight * 1.2 |> int)
    let textWidth = float (heading::lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    let width, height = textWidth + padding*2, textHeight + padding*2
    let x, y = 10, 10

    [ 
        yield! panel (x, y, width, height)
        yield! heading::lines |> List.mapi (fun i line -> 
            let fontSize = if i = 0 then float fontSize * 1.2 |> int else fontSize
            let y = y + padding + (i * lineHeight)
            Text (font, line, (x + padding, y), fontSize, TopLeft, activeColours.text))
    ]

let private tilePopup corpList =
    let qualityLine (corp, quality) =
        sprintf "%s: %i" corp.abbreviation quality
    let head = List.head corpList |> qualityLine
    let rest = match corpList with | _::rest -> rest |> List.map qualityLine | _ -> []
    popup head rest

let private officePopup office corp =
    let corpName = sprintf "%s (%s)" corp.name corp.abbreviation
    let departments = office.departments |> List.map (sprintf "%A")
    popup corpName departments

let private findOfficePopup (mx, my) gameState = 
    let office = 
        allCorps gameState
        |> List.collect (fun c -> allOffices c.headOffice |> List.map (fun o -> o, c))
        |> List.tryFind (fun (o, _) -> o.x = mx && o.y = my)
    office |> Option.bind (fun (o, c) ->
        Some (officePopup o c))

let private findTilePopup (mx, my) gameState = 
    match Map.tryFind (mx, my) gameState.productTiles with
    | Some corpList -> tilePopup corpList
    | _ -> []

let private playerStats gameState =
    let stats = [
        "Cash", gameState.player.cash
        "Income", 0
        "Expenses", 0
        "Change", 0
        "Ideas", gameState.player.ideas
        "Orders Remaining", 0
    ]
    let lines = stats |> List.map (fun (label, value) -> sprintf "%s: %i" label value)

    let x, y, width, height = 10, winh - 140, 200, 130

    [ 
        yield! panel (x, y, width, height)
        yield! lines |> List.mapi (fun i line -> 
            let y = y + padding + (i * lineHeight)
            Text (font, line, (x + padding, y), fontSize, TopLeft, activeColours.text))
    ]

let renderInterface gameState = 
    [
        yield! playerStats gameState

        match gameState.selectedTile with
        | Some (mx, my) ->
            let popup = 
                findOfficePopup (mx, my) gameState 
                |> Option.defaultValue (findTilePopup (mx, my) gameState)
            yield! popup
        | _ -> ()

        yield! getButtonView gameState.buttons.endTurn
    ]