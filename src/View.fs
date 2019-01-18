module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Constants
open Iso
open Turn

let tw, th = tileSize

let lineHeight = float fontSize * 1.3 |> int

let contract n (rx, ry, rw, rh) =
    rx + n, ry + n, rw - 2*n, rh - 2*n

let panel rect = [
    match activeColours.border with
    | Some (n, colour) ->
        yield Colour (rect, colour)
        let inner = contract n rect
        yield Colour (inner, activeColours.background)
    | _ ->
        yield Colour (rect, activeColours.background)
]        

let tilePopup corpList (tx, ty) = [
    let qualityLine (corp, quality) =
        sprintf "%s: %i" corp.abbreviation quality

    let lines = corpList |> List.map qualityLine
    let textHeight = List.length lines * lineHeight
    let textWidth = float (lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    let x, y, _, _ = isoRect tx ty tw th
    let width, height = textWidth + padding*2, textHeight + padding*2
    let x, y = x + tw/2 - width/2, y + th + padding/2

    yield! panel (x, y, width, height)
    yield! lines |> List.mapi (fun i line -> 
        let colour = if i = 0 then activeColours.text else inactiveColours.text
        let y = y + padding + (i * lineHeight)
        Text (font, line, (x + padding, y), fontSize, TopLeft, colour))
]

let officePopup office corp = [
    let lines = [
        yield sprintf "%s (%s)" corp.name corp.abbreviation
        yield! office.departments |> List.map (sprintf "%A")
    ]
    let textHeight = List.length lines * lineHeight
    let textWidth = float (lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    let x, y, _, _ = isoRect office.x office.y tw th
    let width, height = textWidth + padding*2, textHeight + padding*2
    let x, y = x + tw/2 - width/2, y + th + padding/2

    yield! panel (x, y, width, height)
    yield! lines |> List.mapi (fun i line -> 
        let colour = if i = 0 then activeColours.text else inactiveColours.text
        let y = y + padding + (i * lineHeight)
        Text (font, line, (x + padding, y), fontSize, TopLeft, colour))
    ]

let private renderMap (mx, my) gameState =
    let tilePopup = gameState.mouseTile |> Option.defaultValue (-1, -1)
    gameState.map
    |> Set.toList
    |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th

            match Map.tryFind (x, y) gameState.productTiles with
            | Some ((corp, _)::_) -> 
                yield 0, Image ("tile", rect, corp.colour)
            | _ -> 
                yield 0, Image ("tile", rect, Color.White)

            if (x, y) = (mx, my) || (x, y) = tilePopup then
                yield 2, Image ("tile-highlight", rect, Color.White)
        ])

let rec private allOffices office = [
    yield office
    yield! List.collect allOffices office.managedOffices
]

let renderOffice colour mouseTile selectedTile office = 
    [
        let pos = office.x, office.y
        let rect = isoRect office.x office.y tw (th*3)
        yield 1, Image ("office", rect, colour)

        if pos = mouseTile || pos = selectedTile then
            yield 3, Image ("office-highlight", rect, Color.White)

        if pos = selectedTile then
            let tiles = localProductTiles office
            yield! tiles |> List.map (fun (x, y) ->
                let tileRect = isoRect x y tw th
                2, Image ("tile-highlight", tileRect, Color.White))
    ]

let private renderOffices mouseTile gameState =
    let selectedTile = gameState.mouseTile |> Option.defaultValue (-1, -1)
    gameState.corps 
    |> List.collect (fun corp -> 
        allOffices corp.headOffice 
        |> List.collect (renderOffice corp.colour mouseTile selectedTile))

let findOfficePopup (mx, my) gameState = 
    let office = 
        gameState.corps 
        |> List.collect (fun c -> allOffices c.headOffice |> List.map (fun o -> o, c))
        |> List.tryFind (fun (o, _) -> o.x = mx && o.y = my)
    office |> Option.bind (fun (o, c) ->
        Some (officePopup o c))

let findTilePopup (mx, my) gameState = 
    match Map.tryFind (mx, my) gameState.productTiles with
    | Some corpList ->
        tilePopup corpList (mx, my)
    | _ -> []

let renderUI gameState = 
    [
        match gameState.mouseTile with
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

        let mousePos = mouseTile runState |> Option.defaultValue (-1, -1)
        yield! renderMap mousePos gameState
        yield! renderOffices mousePos gameState
        yield! renderUI gameState
        yield 99, getCursor runState
    ] 
    |> List.sortBy fst 
    |> List.map snd 