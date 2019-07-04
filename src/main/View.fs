module Main.View

open Xelmish.Viewables
open Xelmish.Model
open Common
open Iso
open Model
open Pathing
open Orders
open Update

let private renderMarketTiles market =
    market.tiles
    |> Set.toList
    |> List.map (fun (x, y) -> 
        let (tx, ty, tw, th) = isoRect x y tileWidth tileHeight
        let colour = 
            match Map.tryFind (x, y) market.productTiles with 
            | Some ((corp, _)::_) -> corp.colour | _ -> Colour.White
        image "tile" colour (tw, th) (tx, ty))
            
let private renderOfficeLinks market =
    let rec linkOfficeAndManaged office = 
        office.managedOffices
        |> List.collect (fun subOffice ->
            let links =
                match findPathBetween office subOffice market with
                | Some path -> graphicsForPath path []
                | _ -> []
            links @ linkOfficeAndManaged subOffice)

    market.allCorps
    |> List.collect (fun corp -> 
        linkOfficeAndManaged corp.headOffice)

let private renderOffices (market: Market) =
    market.allOffices
    |> List.sortBy (fun info -> info.office.y, -info.office.x)
    |> List.collect (fun info -> 
        let (x, y, w, h) = isoRect info.office.x info.office.y tileWidth (tileHeight*3)
        [   yield image "office" info.corporation.colour (w, h) (x, y)
            if info.headOffice then
                let (x, y, w, h) = isoRect (info.office.x+2) (info.office.y-2) tileWidth tileHeight
                yield colour Colour.White (tileWidth/4, tileHeight/2) (x+(tileWidth/8)*3, y+tileHeight/4) // this background makes the star white
                yield image "icon-head-office" info.corporation.colour (w, h) (x, y) ])

let private renderHighlight (market: Market) (mx, my) = [
        let (x, y, w, h) = isoRect mx my tileWidth tileHeight
        yield image "tile-highlight" Colour.White (w, h) (x, y)
        match market.atTile (mx, my) with
        | Some (OfficeInfo info) ->
            let (x, y, w, h) = isoRect info.office.x info.office.y tileWidth (tileHeight*3)
            yield image "office-highlight" Colour.White (w, h) (x, y)
            yield! info.productTiles
                |> List.map (fun (tx, ty) ->
                    let (x, y, w, h) = isoRect tx ty tileWidth tileHeight
                    image "tile-highlight" Colour.White (w, h) (x, y))
        | _ -> ()            
    ]

let renderMarket model =
    [   yield! renderMarketTiles model.market
        yield! renderOfficeLinks model.market
        yield! renderOffices model.market
        yield! renderHighlight model.market model.selectedTile ]

let renderUserInterface model dispatch = 
    // the user interface is in one of three modes:
    // - general info or default: shows the player's corp, their selected executive, and the selected entity's information
    // - order screen, giving order options
    // - order select screen, giving instructions on what to select (also shows the currently selected entity)
    [   yield OnDraw (fun assets inputs spritebatch ->
        let mouseTile = mouseTile (inputs.mouseState.X, inputs.mouseState.Y)
        if model.market.tiles.Contains mouseTile then 
            renderHighlight model.market mouseTile
            |> List.choose (function OnDraw f -> Some f | _ -> None)
            |> List.iter (fun f -> f assets inputs spritebatch))

        yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            if model.market.tiles.Contains mouseTile then 
                dispatch (SelectTile mouseTile)) ]

let view model dispatch =
    [   yield! renderMarket model
        yield! renderUserInterface model dispatch
        
        yield onkeydown Keys.Escape exit ]