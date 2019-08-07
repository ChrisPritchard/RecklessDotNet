module Main.View_Interface2

open Xelmish.Model
open Xelmish.UI
open Constants
open Model

let corpInfo corporation = 
    col [] [
        yield text [ fontSize 25.; height (pct 0.18) ] corporation.displayName
        yield! [
            "Ideas", string corporation.ideas
            "Prospects", "TODO"
            "Stock Value", "TODO"
            "Market Share", "TODO"
            "Cash", sprintf "$%i" corporation.cash
            "Expenses", "TODO"
        ] |> List.map (fun (label, value) ->
            row [ height (pct 0.13) ] [
                text [] label
                text [] value
            ])
    ]

let executiveInfo executive dispatch = 
    row [] [
        col [] [
            row [ height (pct 0.18) ] [ ]
            text [ height (pct 0.13) ] (sprintf "LVL:   %i" executive.level)
            text [ height (pct 0.13) ] (sprintf "XP:    %i" executive.experience)
            text [ height (pct 0.13) ] executive.lastName

            button [ height (pct 0.21); onclick (fun _ -> ()) ] "Orders"
            button [ height (pct 0.21); onclick (fun _ -> ()); enabled false ] "Corp Report"
        ]
        col [ margin (px defaultMargin); backgroundColour colours.temp ] []
    ]

let departmentLabels departments = 
    let departmentLabel department =
        let cost = departmentCost department
        match department with
        | Marketing -> sprintf  "Marketing      -%i" cost
        | Research -> sprintf   "Research       -%i" cost
        | Admin _ -> sprintf    "Admin          -%i" cost
        | _ -> ""
    let products, other = departments |> List.partition (function Product _ -> true | _ -> false)
    let otherLabels = other |> List.map departmentLabel
    match products with
    | [] -> otherLabels
    | _ -> (sprintf "%i Products" (List.length products))::otherLabels

let selectedInfo selected =
    
    //let split = rowsAndCols [] [0.3] (x, y, w, h)
    //let infoRects = rowsAndCols [0.18;0.13;0.1;0.1;0.1;0.1;0.1;0.1] [] split.[1] |> Array.map topLeft
    
    let tx, ty = px (tileWidth / 2), px (tileHeight*2)
    let tw, th = px tileWidth, px tileHeight

    let left, right =
        match selected with
        | Some (TileInfo ti) -> 
            match ti with 
            | (dominant, quality)::_ ->
                let left = [
                    image [ colour dominant.colour; x tx; y ty; width tw; height th ] "tile"
                ]
                let right = [
                    yield text [ height (pct 0.18); fontSize 25. ] dominant.displayName
                    yield text [ height (pct 0.13) ] (sprintf "Quality        %i" quality)
                    yield row [ height (pct 0.1) ] []
                    yield! 
                        ti  |> List.skip 1 |> List.truncate 5 
                            |> List.map (fun (corp, _) ->
                                text [ height (pct 0.1) ] corp.displayName)
                ]
                left, right                
            | _ -> 
                [ image [ colour Colour.White; x tx; y ty; width tw; height th ] "tile" ],
                [ text [ fontSize 25. ] "Empty Market Tile" ]
        | Some (OfficeInfo oi) ->
            let left = [
                image [ colour oi.corporation.colour; x tx; y ty; width tw; height th ] "tile"
                // todo : office
            ]
            let right = [
                yield text [ height (pct 0.18) ] (sprintf "Cash Flow: %i     Q: %i" 0 oi.quality)
                yield row [ height (pct 0.1) ] []
                yield! oi.office.departments
                |> departmentLabels
                |> List.map (text [ height (pct 0.1) ])
            ]
            left, right
        | _ -> [], []

    row [] [
        col [ width (pct 0.3) ] left
        col [] right
    ]

let commandArea model dispatch = 
    let body = 
        match model.currentInterface with
        | Information selectedTile -> 
            let selected = model.market.atTile selectedTile
            let corp = 
                match selected with 
                | Some (OfficeInfo o) -> o.corporation 
                | Some (TileInfo ((dominant, _)::_)) -> dominant
                | _ -> model.market.player
            let executive = 
                match selected with
                | Some (OfficeInfo o) -> o.executive.Value
                | _ -> corp.ceo
            [
                corpInfo corp
                executiveInfo executive dispatch
                selectedInfo selected
            ]
        | _ -> []

    col [] [
        row [ height (pct 0.7) ] []
        row [ ]
            body
    ]

let renderUserInterface model dispatch = 
    let all = commandArea model dispatch
    let style = {
        backgroundColour = colours.background
        fontName = "defaultFont"
        fontSize = 16.
        colour = colours.text
        margin = px (0)
        padding = px (0)
        borderSize = 0
        borderColour = colours.temp
        alignment = 0., 0.
        enabled = true
    }
    render style (0, 0) (windowWidth, windowHeight) all