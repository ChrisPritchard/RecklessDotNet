module Turn

open Model
open Constants

let getQualityChange office =
    let qualityAssuranceCount =
        office.extensions |> List.sumBy (function QA -> 1)
    let findResearch =
        office.departments 
        |> List.tryFind (function Research -> true | _ -> false) 
    (qualityAssuranceCount * 5) + (match findResearch with Some _ -> 10 | _ -> -10)

let endOfficeTurn orders office =
    
    let qualityChange = getQualityChange office
    let adjusted = 
        office.departments 
        |> List.map (function
            | Product quality -> Product (quality + qualityChange)
            | other -> other )
    { office with departments = adjusted }

let endCorpTurn corp =
    let ordersByOffice = corp.orders |> List.groupBy (fun o -> o.office) |> Map.ofList
    let getOrdersFor o = 
        match ordersByOffice |> Map.tryFind (Some o) with 
        | Some orders -> orders | _ -> []
    let adjustedOffices = corp.offices |> List.map (fun office -> 
        endOfficeTurn (getOrdersFor office) office)

    let totalOrderCost = corp.orders |> List.sumBy (fun o -> orderCost o.orderType)
    let totalOfficeCost = corp.offices |> List.sumBy (fun o -> o.departments |> List.sumBy departmentCost )

    { corp with 
        offices = adjustedOffices
        cash = corp.cash - totalOrderCost - totalOfficeCost
        orders = [] }

let endTurn corps =
    // advance corps
    // advance offices
    // gather all projected tiles
    // assign profits
    corps |> Seq.map endCorpTurn
