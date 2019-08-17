module Main.View_Interface

open Xelmish.Model
open Xelmish.Viewables
open Xelmish.UI
open Constants
open Model

let defaultPadding = padding (px defaultMargin)
let defaultMargin = margin (px defaultMargin)

let corpInfo corporation = 
    col [] [
        yield text [ fontSize 25.; height (pct 0.18); defaultPadding ] corporation.displayName
        yield! [
            "Ideas", string corporation.ideas
            "Prospects", "TODO"
            "Stock Value", "TODO"
            "Market Share", "TODO"
            "Cash", sprintf "$%i" corporation.cash
            "Expenses", "TODO"
        ] |> List.map (fun (label, value) ->
            row [ height (pct 0.13) ] [
                text [ alignment 1. 0.; defaultPadding ] label
                text [ defaultPadding ] value
            ])
    ]

let executiveInfo executive dispatch = 
    row [] [
        col [] [
            row [ height (pct 0.18) ] [ ]
            text [ height (pct 0.13) ] (sprintf "LVL:   %i" executive.level)
            text [ height (pct 0.13) ] (sprintf "XP:    %i" executive.experience)
            text [ height (pct 0.13) ] executive.lastName

            button [ defaultMargin; height (pct 0.21); onclick (fun _ -> ()) ] "Orders"
            button [ defaultMargin; height (pct 0.21); onclick (fun _ -> ()); enabled false ] "Corp Report"
        ]
        col [ defaultMargin; backgroundColour colours.temp ] []
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
    
    let tilePosFor (info: DrawInfo) = info.x + ((info.width - tileWidth) / 2), info.y + (info.height - (tileHeight*2))

    let left, right =
        match selected with
        | Some (TileInfo ti) -> 
            match ti with 
            | (dominant, quality)::_ ->
                let left = [
                    viewable [] (fun info -> image "tile" dominant.colour (tileWidth, tileHeight) (tilePosFor info))
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
                [ viewable [] (fun info -> image "tile" Colour.White (tileWidth, tileHeight) (tilePosFor info)) ],
                [ text [ fontSize 25. ] "Empty Market Tile" ]
        | Some (OfficeInfo oi) ->
            let left = [
                viewables [] (fun info -> [
                    let tx, ty = tilePosFor info
                    yield image "tile" oi.corporation.colour (tileWidth, tileHeight) (tx, ty)
                    yield! View_Market.renderOffice tx (ty - tileHeight * 2) tileWidth (tileHeight * 3) oi.corporation.colour oi.headOffice
                    ])
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

    row [ padding (px 10) ] [
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

    let style = [
        colour colours.text
        backgroundColour colours.background 
        buttonTextColour colours.text
        buttonBackgroundColour colours.button
        buttonDisabledColour colours.buttonDisabled
        buttonHoverColour colours.buttonHover
        buttonPressedColour colours.buttonPressed
    ]

    col [] [
        row [ height (pct 0.7) ] []
        row style body
    ]

let renderUserInterface model dispatch = 
    let all = commandArea model dispatch
    renderUI false "defaultFont" (0, 0) (windowWidth, windowHeight) all