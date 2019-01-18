module Reckless.Interface

open Microsoft.Xna.Framework
open GameCore.GameModel
open Constants
open Model

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

let private tilePopup corpList (tx, ty) = [
    let qualityLine (corp, quality) =
        sprintf "%s: %i" corp.abbreviation quality

    let lines = corpList |> List.map qualityLine
    let textHeight = List.length lines * lineHeight
    let textWidth = float (lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    //let x, y, _, _ = isoRect tx ty tw th
    let width, height = textWidth + padding*2, textHeight + padding*2
    //let x, y = x + tw/2 - width/2, y + th + padding/2
    let x, y = 10, 10

    yield! panel (x, y, width, height)
    yield! lines |> List.mapi (fun i line -> 
        let colour = if i = 0 then activeColours.text else inactiveColours.text
        let y = y + padding + (i * lineHeight)
        Text (font, line, (x + padding, y), fontSize, TopLeft, colour))
]

let private officePopup office corp = [
    let lines = [
        yield sprintf "%s (%s)" corp.name corp.abbreviation
        yield! office.departments |> List.map (sprintf "%A")
    ]
    let textHeight = List.length lines * lineHeight
    let textWidth = float (lines |> List.map Seq.length |> Seq.max) * (float fontSize) * 0.8 |> int
    
    //let x, y, _, _ = isoRect office.x office.y tw th
    let width, height = textWidth + padding*2, textHeight + padding*2
    //let x, y = x + tw/2 - width/2, y + th + padding/2
    let x, y = 10, 10

    yield! panel (x, y, width, height)
    yield! lines |> List.mapi (fun i line -> 
        let colour = if i = 0 then activeColours.text else inactiveColours.text
        let y = y + padding + (i * lineHeight)
        Text (font, line, (x + padding, y), fontSize, TopLeft, colour))
    ]

let private findOfficePopup (mx, my) gameState = 
    let office = 
        gameState.corps 
        |> List.collect (fun c -> allOffices c.headOffice |> List.map (fun o -> o, c))
        |> List.tryFind (fun (o, _) -> o.x = mx && o.y = my)
    office |> Option.bind (fun (o, c) ->
        Some (officePopup o c))

let private findTilePopup (mx, my) gameState = 
    match Map.tryFind (mx, my) gameState.productTiles with
    | Some corpList ->
        tilePopup corpList (mx, my)
    | _ -> []

let render gameState = 
    [
        match gameState.mouseTile with
        | Some (mx, my) ->
            let popup = 
                findOfficePopup (mx, my) gameState 
                |> Option.defaultValue (findTilePopup (mx, my) gameState)
            yield! popup
        | _ -> ()
    ] |> List.map (fun va -> 90, va)