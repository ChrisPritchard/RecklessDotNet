module Turn

open Model
open Constants

let applyOrder office =
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
    let adjustedOffice = orders |> List.fold applyOrder office
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

let endTurn corps =
    // advance corps
    // advance offices
    // gather all projected tiles
    // assign profits
    corps |> Seq.map endCorpTurn
