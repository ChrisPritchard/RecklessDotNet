module Main

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
with
    member office.pos = (office.x, office.y)
    member office.productTiles market = 
        let products = office.departments |> List.sumBy (function Product _ -> 1 | _ -> 0)
        [-products..products] |> List.collect (fun x ->
        [-products..products] |> List.map (fun y -> x, y))
        |> List.filter (fun (x, y) -> abs x + abs y <= products)
        |> List.map (fun (x, y) -> office.x + x, office.y + y)
        |> List.filter (fun p -> Set.contains p market.tiles)
    member office.productQuality parentOffice =
        let quality, localMarketing = 
            ((0, 0), office.departments) 
            ||> List.fold (fun (q, m) -> 
                function
                | Product v -> q + v, m
                | Marketing -> q, m + 1
                | _ -> q, m)
        let parentMarketing = 
            match parentOffice with 
            | Some o -> o.departments |> List.sumBy (function Marketing -> 1 | _ -> 0) 
            | _ -> 0
        quality * pown 2 (localMarketing + parentMarketing)
and Department =
    | Product of quality: int 
    | Marketing
    | Research
    | Admin of Executive option
and Market = {
    tiles: Set<int * int>
    player: Corporation
    others: Corporation list
}
with 
    member market.allCorps = market.player::market.others    
    member market.allOffices =
        let rec allOffices parentOffice parentExec (office: Office) = 
            let exec = 
                office.departments 
                |> List.tryPick (fun dep -> match dep with Admin (Some e) -> Some (Some e) | _ -> None)
                |> Option.defaultValue parentExec
            (office, office.productQuality parentOffice, exec)::(List.collect (allOffices (Some office) exec) office.managedOffices)
        market.allCorps 
        |> List.collect (fun corp -> 
            allOffices None None corp.headOffice 
            |> List.map (fun (office, quality, exec) -> 
                office, office.productTiles market, quality, corp, exec))
    member market.productTiles = 
        market.allOffices
        |> List.collect (fun (_, tiles, quality, corp, _) ->
            tiles |> List.map (fun tile -> tile, (corp, quality)))
        |> List.groupBy fst
        |> List.map (fun (tile, list) -> 
            tile, List.map snd list |> List.sortByDescending snd)
        |> Map.ofList
    member market.atTile p =
        if not (Set.contains p market.tiles) then None
        else
            match market.allOffices |> List.tryFind (fun (office, _, _, _, _) -> office.pos = p) with
            | Some (office, tiles, quality, corp, exec) -> 
                Some (OfficeInfo (office, tiles, quality, corp, exec))
            | None ->
                let info = Map.tryFind p market.productTiles |> Option.defaultValue []
                Some (TileInfo info)
and Info =
    | OfficeInfo of office:Office * tiles: (int * int) list * quality:int * corp:Corporation * exec:Executive option
    | TileInfo of (Corporation * int) list

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
                departments = [Product 16; Marketing; Product 30; Admin (Some { name = "Christopher Aquinas" })]
            }
            colour = Colour.Yellow
        }
        others = [] }
    { market = market; selectedTile = None }

type Message = 
    | SelectTile of int * int
    | DeselectTile

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

let officeInfoWindowFor office corp (exec: Executive option) =
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
            yield colour Colour.LightGray (300, 100) (320, 10)
            yield text 16. Colour.Black (0., 0.) "managing executive:" (330, 20)
            yield text 18. Colour.Black (0., 0.) executive.name (330, 40)

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
                | Some (OfficeInfo (office, _, _, corp, exec)) -> officeInfoWindowFor office corp exec
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

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = Some (x, y) }, Cmd.none
    | DeselectTile -> { model with selectedTile = None }, Cmd.none