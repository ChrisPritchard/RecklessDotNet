module Main.View

open Xelmish.Viewables
open Xelmish.Model
open Common
open Iso
open Model
open Orders
open Update

let private renderMarket market =
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

let text = text "defaultFont"

let button s event (width, height) (x, y) = 
    [   colour Colour.Blue (width, height) (x, y)
        text 20. Colour.White (-0.5, -0.5) s (x + width/2, y+height/2)
        onclick event (width, height) (x, y) ]

let officeInfoWindowFor market officeInfo dispatch =
    [   yield setSmoothSampling ()

        // general info
        yield colour Colour.LightGray (300, 60 + officeInfo.office.departments.Length * 20) (10, 10)
        let heading = 
            sprintf "%sOffice of %s (%s)" 
                (if officeInfo.headOffice then "Head " else "")
                officeInfo.corporation.name 
                officeInfo.corporation.abbreviation
        yield text 18. Colour.Black (0., 0.) heading (20, 20)
        yield text 16. Colour.Black (0., 0.) "departments:" (20, 40)
        yield! officeInfo.office.departments
            |> List.mapi (fun i dep ->
                let label = "  " +
                            match dep with
                            | Product n -> sprintf "Product (quality: %i)" n
                            | Admin (Some _) -> "Admin (occupied)"
                            | Admin _ -> "Admin (unoccupied)"
                            | other -> string other
                text 16. Colour.Black (0., 0.) label (20, 60 + i * 20))

        // exec info
        match officeInfo.executive with
        | None -> ()
        | Some executive ->
            yield colour Colour.LightGray (300, 140) (320, 10)
            yield text 16. Colour.Black (0., 0.) "Managing Executive:" (330, 20)
            yield text 18. Colour.Black (0., 0.) executive.name (330, 40)
            if officeInfo.corporation = market.player then
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

let contains (x, y) (tx, ty) (tw, th) = 
    x >= tx && x < tx + tw
    && y >= ty && y < ty + th

// What does executive offer here? Should it be scoped by exec?
// Execs need to be tied to orders so they gain experience...todo?
let renderOrderWindow market _ dispatch =

    // should probably list all orders with valid yes/no. Matches base game with greyed out orders
    let validOrders = validOrdersFor market.player

    let size = 300, 140
    let x, y = 640, 10
    let buttonWidth, buttonHeight = 150, 30

    [   yield colour Colour.LightGray size (x, y)

        yield! 
            validOrders 
            |> List.mapi (fun i order -> 
                let orderMessage = TargetOrder order
                let buttonX = x + 10 + (buttonWidth * i)
                let buttonY = y + 10 + ((buttonHeight + 10) * (i % 3))
                button order.displayName (fun () -> dispatch orderMessage) (buttonWidth, buttonHeight) (buttonX, buttonY))
            |> List.collect id
        
        yield onclickpoint (fun mousePoint ->
            if not (contains mousePoint (x, y) size) then
                dispatch CloseWindow)
        ]

let renderMarker model = [

    yield! renderMarket model.market
    yield! renderOfficeLinks model.market
    yield! renderOffices model.market

    match model.selectedTile with
    | None -> () 
    | Some tile -> 
        yield! renderHighlight model.market tile

    ]

let renderUserInterface model dispatch = [

    // the user interface is in one of three modes:
    // - general info or default: shows the player's corp, their selected executive, and the selected entity's information (default to selecting the headquarters? no no-select mode?)
    // - order screen, giving order options
    // - order select screen, giving instructions on what to select (also shows the currently selected entity)

    match model.selectedTile with
    | None -> ()
    | Some tile ->
        yield! 
            match model.market.atTile tile with
            | Some (OfficeInfo info) -> officeInfoWindowFor model.market info dispatch
            | Some (TileInfo owners) -> tileInfoWindowFor owners
            | _ -> []

    match model.window with
    | None ->
        yield OnDraw (fun assets inputs spritebatch ->
            let mouseTile = mouseTile (inputs.mouseState.X, inputs.mouseState.Y)
            if model.market.tiles.Contains mouseTile then 
                renderHighlight model.market mouseTile
                |> List.choose (function OnDraw f -> Some f | _ -> None)
                |> List.iter (fun f -> f assets inputs spritebatch))

        yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            if model.market.tiles.Contains mouseTile then 
                dispatch (SelectTile mouseTile))
    | Some (SelectOrder executive) ->
        yield! renderOrderWindow model.market executive dispatch
    
    ]

let view model dispatch =
    [
        yield! renderMarket model.market
        yield! renderUserInterface model dispatch
        
        yield onkeydown Keys.Escape exit
    ]