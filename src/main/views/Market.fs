module Main.Views.Market

open Xelmish.Model
open Xelmish.Viewables
open Constants
open IsoHelper
open Main.Model
open Main.Update

let private renderMarketTiles market =
    market.tiles
    |> Set.toList
    |> List.map (fun (x, y) -> 
        let (tx, ty, tw, th) = isoRect x y tileWidth tileHeight
        let colour = 
            match Map.tryFind (x, y) market.productTiles with 
            | Some ((corp, _)::_) -> corp.colour | _ -> Colour.White
        image "tile" colour (tw, th) (tx, ty))

let findPathBetween (sourceOffice: Office) (destOffice: Office) (market: Market) =
    let rec bfs paths visited =
        let next = 
            paths
            |> List.collect (fun path ->
                let (x, y) = List.head path
                [-1, 0; 1, 0; 0, -1; 0, 1]
                |> List.map (fun (dx, dy) -> x + dx, y + dy)
                |> List.filter (fun p -> not (Set.contains p visited))
                |> List.map (fun p -> p::path))
        if next = [] then None 
        else
            match List.tryFind (fun path -> List.head path = destOffice.pos) next with
            | Some path -> Some path
            | _ ->
                let newHeads = paths |> List.map List.head
                let newVisited = List.foldBack Set.add newHeads visited
                bfs next newVisited
    let startVisited = 
        market.allOffices 
        |> List.filter (fun info -> info.office <> destOffice) 
        |> List.map (fun info -> info.office.pos)
        |> Set.ofList
    bfs [[sourceOffice.pos]] startVisited

let rec graphicsForPath path soFar =
    let sprite (px, py) (x, y) =
        OnDraw (fun loadedAssets _ (spriteBatch: SpriteBatch) ->
            let texture = loadedAssets.textures.["office-links"]

            let tx, ty, tw, th = isoRect x y tileWidth tileHeight
            let otx, oty = int (px * float tw), int (py * float th)
            let destRect = rect (tx + otx) (ty + oty) (tw/2) (th/2)

            let sx, sy = int (px * float texture.Width), int (py * float texture.Height)
            let sourceRect = System.Nullable(rect sx sy (texture.Width/2) (texture.Height/2))
            
            spriteBatch.Draw (texture, destRect, sourceRect, Colour.White))

    match path with
    | (cx, cy)::(nx, ny)::rest ->
        let centre = 
            let tx, ty, tw, th = isoRect cx cy tileWidth tileHeight
            image "office-link-centre" Colour.White (tw, th) (tx, ty)
        let current, next =
            if cx = nx && cy < ny then
                sprite (0.5, 0.5) (cx, cy), sprite (0., 0.) (nx, ny)
            elif cx = nx && cy > ny then
                sprite (0., 0.) (cx, cy), sprite (0.5, 0.5) (nx, ny)
            elif cx < nx then
                sprite (0.5, 0.) (cx, cy), sprite (0., 0.5) (nx, ny)
            else
                sprite (0., 0.5) (cx, cy), sprite (0.5, 0.) (nx, ny)
        graphicsForPath ((nx, ny)::rest) (centre::current::next::soFar)
    | _ -> soFar

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

let renderOffice x y w h corpColour isHeadOffice = [
    yield image "office" corpColour (w, h) (x, y)
    if isHeadOffice then
        let (x, y, w, h) = x, y, tileWidth, tileHeight
        yield colour Colour.White (tileWidth/4, tileHeight/2) (x+(tileWidth/8)*3, y+tileHeight/4) // this background makes the star white
        yield image "icon-head-office" corpColour (w, h) (x, y)
    ]

let private renderOffices (market: Market) =
    market.allOffices
    |> List.sortBy (fun info -> info.office.y, -info.office.x)
    |> List.collect (fun info -> 
        let (x, y, w, h) = isoRect info.office.x info.office.y tileWidth (tileHeight*3)
        renderOffice x y w h info.corporation.colour info.headOffice)

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

let renderMarket model dispatch =
    [   yield! renderMarketTiles model.market
        yield! renderOfficeLinks model.market
        yield! renderOffices model.market
        
        match model.playerAction, model.selectedTile with
        | Overview, Some selectedTile ->
            yield! renderHighlight model.market selectedTile
        | _ -> ()

        yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            if model.market.tiles.Contains mouseTile then 
                dispatch (SelectTile mouseTile))
    ]