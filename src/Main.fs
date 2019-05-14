﻿module Main

open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Constants
open Iso

type Executive = {
    name: string
}

type Corporation = {
    name: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    colour: Colour
}
and Office = {
    x: int
    y: int
    managedOffices: Office list
    departments: Department list
}
and Department =
    | Product of quality: int 
    | Marketing
    | Research
    | Admin of Executive option

type Market = {
    tiles: Set<int * int>
    player: Corporation
    others: Corporation list
}

type Model = {
    market: Market
    selectedTile: (int * int) option
}

let init () = 
    let market = {
        tiles = 
            [0..(10*10)-1] 
            |> List.fold (fun map i -> 
                let x = i % 10
                let y = i / 10
                Set.add (x, y) map) Set.empty
        player = {   
            name = "Aquinas Corp"
            abbreviation = "AQU"
            cash = 5000
            ideas = 0
            headOffice = {
                x = 2
                y = 7
                managedOffices = []
                departments = [Product 16; Marketing; Product 30]
            }
            colour = Colour.Yellow
        }
        others = [] }
    { market = market; selectedTile = None }

type Message = 
    | SelectTile of int * int
    | DeselectTile


let rec allOffices office = office::List.collect allOffices office.managedOffices

let officeProductTiles office = 
    let products = office.departments |> List.sumBy (function Product _ -> 1 | _ -> 0)
    [-products..products] |> List.collect (fun x ->
    [-products..products] |> List.map (fun y -> x, y))
    |> List.filter (fun (x, y) -> abs x + abs y <= products)
    |> List.map (fun (x, y) -> office.x + x, office.y + y)

let corpProductTiles corp = 
    let rec gatherer parent office = 
        let quality, localMarketing = 
            ((0, 0), office.departments) 
            ||> List.fold (fun (q, m) -> 
                function
                | Product v -> q + v, m
                | Marketing -> q, m + 1
                | _ -> q, m)
        let parentMarketing = 
            match parent with 
            | Some o -> o.departments |> List.sumBy (function Marketing -> 1 | _ -> 0) 
            | _ -> 0
        let quality = quality * pown 2 (localMarketing + parentMarketing)
        [
            yield! officeProductTiles office |> List.map (fun (x, y) -> x, y, quality)
            yield! List.collect (gatherer (Some office)) office.managedOffices
        ]
    gatherer None corp.headOffice

let private renderMarket productTiles model =
    model.tiles
    |> Set.toList
    |> List.map (fun (x, y) -> 
        let (x, y, w, h) = isoRect x y tileWidth tileHeight
        let colour = 
            match Map.tryFind (x, y) productTiles with 
            | Some ((corp, _)::_) -> corp.colour | _ -> Colour.White
        image "tile" colour (w, h) (x, y))

let allCorps gameState = gameState.player::gameState.others

let gameProductTiles gameState =
    allCorps gameState
    |> List.collect (fun c -> corpProductTiles c |> List.map (fun (x, y, q) -> (x, y), (c, q)))
    |> List.groupBy fst
    |> List.map (fun (pos, tiles) -> 
        let ordered = 
            tiles 
            |> List.sortByDescending (fun (_, (_, q)) -> q) 
            |> List.map snd
        pos, ordered)
    |> Map.ofList

let private renderOffices gameState =
    allCorps gameState
    |> List.collect (fun corp -> allOffices corp.headOffice |> List.map (fun o -> corp, o))
    |> List.sortBy (fun (_, office) -> office.y, -office.x)
    |> List.map (fun (corp, office) -> 
        let (x, y, w, h) = isoRect office.x office.y tileWidth (tileHeight*3)
        image "office" corp.colour (w, h) (x, y))

let private renderHighlight model (mx, my) = [
        let (x, y, w, h) = isoRect mx my tileWidth tileHeight
        yield image "tile-highlight" Colour.White (w, h) (x, y)

        let allOffices = allCorps model |> List.collect (fun corp -> allOffices corp.headOffice)
        match List.tryFind (fun office -> (office.x, office.y) = (mx, my)) allOffices with
        | None -> ()
        | Some office ->
            let (x, y, w, h) = isoRect office.x office.y tileWidth (tileHeight*3)
            yield image "office-highlight" Colour.White (w, h) (x, y)

            let tiles = officeProductTiles office
            yield! tiles 
                |> List.filter (fun p -> Set.contains p model.tiles) 
                |> List.map (fun (tx, ty) ->
                    let (x, y, w, h) = isoRect tx ty tileWidth tileHeight
                    image "tile-highlight" Colour.White (w, h) (x, y))
    ]

let view model dispatch =
    [
        let productTiles = gameProductTiles model.market
        yield! renderMarket productTiles model.market
        yield! renderOffices model.market

        match model.selectedTile with
        | None -> () | Some tile -> yield! renderHighlight model.market tile

        yield OnDraw (fun assets inputs spritebatch ->
            let mouseTile = mouseTile (inputs.mouseState.X, inputs.mouseState.Y)
            if model.market.tiles.Contains mouseTile then 
                let posText = text "defaultFont" 18. Colour.Black (0., 0.) (sprintf "%A" mouseTile) (10, 10)
                let highlight = renderHighlight model.market mouseTile
                (posText::highlight)
                |> List.choose (function OnDraw f -> Some f | _ -> None)
                |> List.iter (fun f -> f assets inputs spritebatch))

        yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            let message = 
                if model.market.tiles.Contains mouseTile 
                then SelectTile mouseTile else DeselectTile
            dispatch message)

        yield onkeydown Keys.Escape exit
    ]

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = Some (x, y) }, Cmd.none
    | DeselectTile -> { model with selectedTile = None }, Cmd.none