module Market

open Elmish
open Xelmish.Model
open Xelmish.Viewables
open Constants
open SharedModel
open Helpers
open Iso

type Model = {
    market: Set<int * int>
    player: Corporation
    others: Corporation list
    selectedTile: (int * int) option
}

let init () = 
    {
        market = StartModel.startMarket
        player = StartModel.player
        others = StartModel.others
        selectedTile = None
        //phase = Orders false
    }

type Message = SelectTile of x:int * y:int

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = Some (x, y) }, Cmd.none

let private renderMarket productTiles gameState =
    gameState.market
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

let private renderHighlight gameState (mx, my) = [
        let (x, y, w, h) = isoRect mx my tileWidth tileHeight
        yield image "tile-highlight" Colour.White (w, h) (x, y)

        let allOffices = allCorps gameState |> List.collect (fun corp -> allOffices corp.headOffice)
        match List.tryFind (fun office -> (office.x, office.y) = (mx, my)) allOffices with
        | None -> ()
        | Some office ->
            let (x, y, w, h) = isoRect office.x office.y tileWidth (tileHeight*3)
            yield image "office-highlight" Colour.White (w, h) (x, y)

            let tiles = officeProductTiles office
            yield! tiles 
                |> List.filter (fun p -> Set.contains p gameState.market) 
                |> List.map (fun (tx, ty) ->
                    let (x, y, w, h) = isoRect tx ty tileWidth tileHeight
                    image "tile-highlight" Colour.White (w, h) (x, y))
    ]

let view model dispatch =
    [
        let productTiles = gameProductTiles model
        yield! renderMarket productTiles model
        yield! renderOffices model

        match model.selectedTile with
        | None -> () | Some tile -> yield! renderHighlight model tile

        yield OnDraw (fun assets inputs spritebatch ->
            let mouseTile = mouseTile (inputs.mouseState.X, inputs.mouseState.Y)
            if model.market.Contains mouseTile then 
                let posText = text "defaultFont" 18. Colour.Black (0., 0.) (sprintf "%A" mouseTile) (10, 10)
                let highlight = renderHighlight model mouseTile
                (posText::highlight)
                |> List.choose (function OnDraw f -> Some f | _ -> None)
                |> List.iter (fun f -> f assets inputs spritebatch))

        yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            if model.market.Contains mouseTile then 
                dispatch (SelectTile mouseTile))

        yield onkeydown Keys.Escape exit
    ]