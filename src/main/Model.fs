module Main.Model

open Xelmish.Model

type Executive = {
    name: string
}

type Corporation = {
    displayName: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    colour: Colour
}
with member corp.allOffices =
        let rec allOffices parentOffice parentExec (office: Office) = 
            let exec = 
                office.departments 
                |> List.tryPick (fun dep -> match dep with Admin (Some e) -> Some (Some e) | _ -> None)
                |> Option.defaultValue parentExec
            (office, office.productQuality parentOffice, exec)::(List.collect (allOffices (Some office) exec) office.managedOffices)
        allOffices None None corp.headOffice
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
        market.allCorps 
        |> List.collect (fun corp -> 
            corp.allOffices
            |> List.map (fun (office, quality, exec) -> 
                {   office = office
                    productTiles = office.productTiles market
                    quality = quality
                    corporation = corp
                    executive = exec
                    headOffice = office = corp.headOffice }))
    member market.productTiles = 
        market.allOffices
        |> Seq.collect (fun info ->
            info.productTiles |> Seq.map (fun tile -> tile, (info.corporation, info.quality)))
        |> Seq.groupBy fst
        |> Seq.map (fun (tile, list) -> 
            let corps = 
                Seq.map snd list 
                |> Seq.sortByDescending snd
                |> Seq.distinctBy fst
            tile, Seq.toList corps)
        |> Map.ofSeq
    member market.atTile p =
        if not (Set.contains p market.tiles) then None
        else
            match market.allOffices |> List.tryFind (fun info -> info.office.pos = p) with
            | Some officeInfo -> 
                Some (OfficeInfo officeInfo)
            | None ->
                let info = Map.tryFind p market.productTiles |> Option.defaultValue []
                Some (TileInfo info)
and Info =
    | OfficeInfo of OfficeInfo
    | TileInfo of (Corporation * int) list
and OfficeInfo = 
    {   office:Office
        productTiles: (int * int) list
        quality:int
        corporation:Corporation
        executive:Executive option
        headOffice: bool    }

type MainModel = 
    {   market: Market
        currentInterface: Interface  }
and Interface =
    | Information of selectedTile: (int * int)
    | OrderTypeSelect
    | TargetOrder