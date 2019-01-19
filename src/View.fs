module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Constants
open Model
open Iso

let private renderMarket currentMouseTile gameState =
    let selectedTile = gameState.selectedTile |> Option.defaultValue (-1, -1)
    gameState.market
    |> Set.toList
    |> List.collect (fun (x, y) -> [
        let rect = isoRect x y tw th

        match Map.tryFind (x, y) gameState.productTiles with
        | Some ((corp, _)::_) -> 
            yield 0, Image ("tile", rect, corp.colour)
        | _ -> 
            yield 0, Image ("tile", rect, Color.White)

        if (x, y) = selectedTile || (x, y) = currentMouseTile then
            yield 2, Image ("tile-highlight", rect, Color.White)
    ])

let private renderOffice colour currentMouseTile gameState office = 
    [
        let pos = office.x, office.y
        let rect = isoRect office.x office.y tw (th*3)
        yield 1, Image ("office", rect, colour)
        
        let selectedTile = gameState.selectedTile |> Option.defaultValue (-1, -1)    

        if pos = selectedTile || pos = currentMouseTile then
            yield 3, Image ("office-highlight", rect, Color.White)

        if pos = selectedTile then
            let tiles = productTiles office
            yield! tiles |> List.filter (fun p -> Set.contains p gameState.market) |> List.map (fun (x, y) ->
                let tileRect = isoRect x y tw th
                2, Image ("tile-highlight", tileRect, Color.White))
    ]

let private renderOffices currentMouseTile gameState =
    allCorps gameState
    |> List.collect (fun corp -> 
        allOffices corp.headOffice 
        |> List.collect (renderOffice corp.colour currentMouseTile gameState))

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

let renderUI gameState = 
    [
        match gameState.selectedTile with
        | Some (mx, my) ->
            let popup = 
                findOfficePopup (mx, my) gameState 
                |> Option.defaultValue (findTilePopup (mx, my) gameState)
            yield! popup
        | _ -> ()
    ] |> List.map (fun va -> 90, va)

let private getCursor runState =
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))

let getView runState gameState = 
    [
        // render map and corps
        // render popups (office info, tile info)
        // render ui commands (orders, end turn)
        // render status (cash, cashflow, ideas)
        // render mouse cursor

        let mousePos = mouseTile runState
        yield! renderMarket mousePos gameState
        yield! renderOffices mousePos gameState
        yield! renderUI gameState
        yield 99, getCursor runState
    ] 
    |> List.sortBy fst 
    |> List.map snd 