module Turn

open Model
open Constants

let applyOfficeOrder office =
    function
    | BuildDepartment dep -> { office with departments = dep::office.departments }
    | BuildExtension ext -> { office with extensions = ext::office.extensions }
    | _ -> office

let getQualityChange office =
    let qualityAssuranceCount =
        office.extensions |> List.sumBy (function QA -> 1)
    let findResearch =
        office.departments 
        |> List.tryFind (function Research -> true | _ -> false) 
    (qualityAssuranceCount * 5) + (match findResearch with Some _ -> 10 | _ -> -10)

let endOfficeTurn orders office =
    let adjustedOffice = orders |> List.fold applyOfficeOrder office
    let qualityChange = getQualityChange office
    let adjustedDepartments = 
        office.departments 
        |> List.map (function
            | Product quality -> Product (quality + qualityChange)
            | other -> other )
    { adjustedOffice with 
        departments = adjustedDepartments }

let endCorpTurn corp =
    let ordersByOffice = corp.orders |> List.groupBy (fun o -> o.office) |> Map.ofList
    let getOrdersFor o = 
        match ordersByOffice |> Map.tryFind (Some o) with 
        | Some orders -> orders |> List.map (fun ord -> ord.orderType) | _ -> []

    let adjustedOffices = corp.offices |> List.map (fun office -> 
        endOfficeTurn (getOrdersFor office) office)

    let totalOrderCost = corp.orders |> List.sumBy (fun o -> orderCost o.orderType)
    let totalOfficeCost = corp.offices |> List.sumBy (fun o -> o.departments |> List.sumBy departmentCost )

    let newIdeas = corp.orders |> List.sumBy (fun o -> if o.orderType = ResearchIdea then 1 else 0)

    { corp with 
        offices = adjustedOffices
        cash = corp.cash - totalOrderCost - totalOfficeCost
        ideas = corp.ideas + newIdeas
        orders = [] }

let marketTilesFor parentMarketing office =
    let (radius, baseQuality, localMarketing) =
        office.departments |> List.fold (fun (r, q, m) -> 
            function
            | Product quality -> r + 1, q + quality, m
            | Marketing -> r, q, m + 1
            | _ -> r, q, m) (0, 0, 0)
    let quality = baseQuality * pown 2 (localMarketing + parentMarketing)
    
    [-radius..radius] |> List.collect (fun x ->
    [-radius..radius] |> List.map (fun y -> (x, y, quality)))

let endTurn corps =
    let adjustedCorps = 
        corps |> List.map endCorpTurn
    let allTiles = corps |> List.collect (fun c -> 
        c.offices 
        |> List.collect (marketTilesFor 0) 
        |> List.map (fun (x, y, q) -> (x, y), (q, c))
        |> List.sortBy (fun (_, (q, _)) -> q))
    let tilesPerCorp = 
        allTiles 
        |> List.fold (fun map (p, (_, c)) -> Map.add p c map) Map.empty 
        |> Map.toList
        |> List.countBy snd
        |> Map.ofList
    adjustedCorps |> List.map (fun corp ->
        let count = Map.tryFind corp tilesPerCorp |> Option.defaultValue 0
        { corp with cash = corp.cash + (count * 100) })