module Main.Model

open Xelmish.Model

type Executive = {
    firstName: string
    lastName: string
    experience: int
    level: int
}

type Corporation = {
    displayName: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    colour: Colour
}
with 
    member corp.allOffices =
        let rec allOffices parentOffice parentExec (office: Office) = 
            let exec = 
                office.departments 
                |> List.tryPick (fun dep -> match dep with Admin (Some e) -> Some (Some e) | _ -> None)
                |> Option.defaultValue parentExec
            (office, office.productQuality parentOffice, exec)::(List.collect (allOffices (Some office) exec) office.managedOffices)
        allOffices None None corp.headOffice
    member corp.ceo =
        let (_, _, exec) = List.head corp.allOffices
        exec.Value
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
    | HR
    | Acquisitions
    | Legal
    | Security
    | ComputerCore
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

type Order = 
    {   displayName: string
        components: OrderComponent list }
and OrderComponent =
    | CorpTransform of condition:(Corporation -> bool) * action:(Corporation -> Corporation)
    | OfficeTransform of condition:(Office -> bool -> bool) * action:(Office -> Office)

let defaultOrderCategory = "Corporate"

type MainModel = 
    {   market: Market
        newInterfaceMode: bool
        currentInterface: Interface  }
and Interface =
    | Information of selectedTile: (int * int)
    | OrderTypeSelect of activeCategory: string
    | TargetOrder of Order

let departmentCost = 
    function
    | Research -> 500
    | Marketing -> 500
    | Admin _ -> 500
    | Product _ -> 0 
    | HR -> 500 // TODO: source real values
    | Acquisitions -> 1000 // TODO: source real values
    | Legal -> 1000 // TODO: source real values
    | Security -> 1000 // TODO: source real values
    | ComputerCore -> 1000 // TODO: source real values