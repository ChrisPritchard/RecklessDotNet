module Main.View

open Xelmish.Viewables
open Xelmish.Model
open Constants
open Iso
open Model

let private renderMarket market =
    market.tiles
    |> Set.toList
    |> List.map (fun (x, y) -> 
        let (tx, ty, tw, th) = isoRect x y tileWidth tileHeight
        let colour = 
            match Map.tryFind (x, y) market.productTiles with 
            | Some ((corp, _)::_) -> corp.colour | _ -> Colour.White
        image "tile" colour (tw, th) (tx, ty))

let private renderOffices (market: Market) =
    market.allOffices
    |> List.sortBy (fun (office, _, _, _, _) -> office.y, -office.x)
    |> List.map (fun (office, _, _, corp, _) -> 
        let (x, y, w, h) = isoRect office.x office.y tileWidth (tileHeight*3)
        image "office" corp.colour (w, h) (x, y))

let private renderHighlight (market: Market) (mx, my) = [
        let (x, y, w, h) = isoRect mx my tileWidth tileHeight
        yield image "tile-highlight" Colour.White (w, h) (x, y)
        match market.atTile (mx, my) with
        | Some (OfficeInfo (office, tiles, _, _, _)) ->
            let (x, y, w, h) = isoRect office.x office.y tileWidth (tileHeight*3)
            yield image "office-highlight" Colour.White (w, h) (x, y)
            yield! tiles
                |> List.map (fun (tx, ty) ->
                    let (x, y, w, h) = isoRect tx ty tileWidth tileHeight
                    image "tile-highlight" Colour.White (w, h) (x, y))
        | _ -> ()            
    ]

let text = text "defaultFont"

let button s event (width, height) (x, y) = 
    [
        colour Colour.Blue (width, height) (x, y)
        text 20. Colour.White (-0.5, -0.5) s (x + width/2, y+height/2)
        onclick event (width, height) (x, y)
    ]

let officeInfoWindowFor office corp (exec: Executive option) dispatch =
    [   yield setSmoothSampling ()

        // general info
        yield colour Colour.LightGray (300, 60 + office.departments.Length * 20) (10, 10)
        let heading = sprintf "Office of %s (%s)" corp.name corp.abbreviation
        yield text 18. Colour.Black (0., 0.) heading (20, 20)
        yield text 16. Colour.Black (0., 0.) "departments:" (20, 40)
        yield! office.departments
            |> List.mapi (fun i dep ->
                let label = "  " +
                            match dep with
                            | Product n -> sprintf "Product (quality: %i)" n
                            | Admin (Some _) -> "Admin (occupied)"
                            | Admin _ -> "Admin (unoccupied)"
                            | other -> string other
                text 16. Colour.Black (0., 0.) label (20, 60 + i * 20))

        // exec info
        match exec with
        | None -> ()
        | Some executive ->
            yield colour Colour.LightGray (300, 140) (320, 10)
            yield text 16. Colour.Black (0., 0.) "managing executive:" (330, 20)
            yield text 18. Colour.Black (0., 0.) executive.name (330, 40)
            let orderMessage = ShowWindow (SelectOrder executive)
            yield! button "Give Order" (fun () -> dispatch orderMessage) (220, 30) (360, 110)

        yield setPixelSampling () ]
    
let tileInfoWindowFor owners =
    [   yield setSmoothSampling ()
        yield colour Colour.LightGray (300, 60 + List.length owners * 20) (10, 10)
        yield text 18. Colour.Black (0., 0.) "Market tile" (20, 20)
        match owners with
        | [] ->
            yield text 16. Colour.Black (0., 0.) "no corporations influence this tile" (20, 40)
        | _ ->
            yield text 16. Colour.Black (0., 0.) "products being sold here:" (20, 40)
            yield! owners
                |> List.mapi (fun i (corp, quality) ->
                    let label = 
                        if i = 0 then
                            sprintf "  %s (dominant): %i quality" corp.name quality
                        else
                            sprintf "  %s: %i quality" corp.name quality
                    text 16. Colour.Black (0., 0.) label (20, 60 + i * 20))
        yield setPixelSampling () ]

let view model dispatch =
    [
        yield! renderMarket model.market
        yield! renderOffices model.market

        match model.selectedTile with
        | None -> () 
        | Some tile -> 
            yield! renderHighlight model.market tile
            yield! 
                match model.market.atTile tile with
                | Some (OfficeInfo (office, _, _, corp, exec)) -> officeInfoWindowFor office corp exec dispatch
                | Some (TileInfo owners) -> tileInfoWindowFor owners
                | _ -> []

        yield OnDraw (fun assets inputs spritebatch ->
            let mouseTile = mouseTile (inputs.mouseState.X, inputs.mouseState.Y)
            if model.market.tiles.Contains mouseTile then 
                renderHighlight model.market mouseTile
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