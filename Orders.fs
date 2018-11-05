module Orders

open Model
open Constants

let validateOrder orderType target = 
    match orderType with
    | BuildDepartment _ -> 
        if target.departments.Length = maxDepartments then Error "max departments reached"
        else Ok ()
    | DownsizeDepartment department ->
        if not <| List.contains department target.departments then Error "no matching department in target"
        else Ok ()
    | MoveExecutive executive ->
        if List.contains (TopFloor None) target.departments |> not then Error "no empty executive department"
        elif executive.owner <> target.owner then Error "un-owned executive"
        else Ok ()

let issueOrder order executive = 
    if executive.orders.Length > int executive.level then Error "too many orders"
    elif order.target.owner <> executive.owner then Error "un-owned target"
    else
        match validateOrder order.orderType order.target with
        | Ok () -> Ok { executive with orders = order::executive.orders }
        | Error s -> Error <| sprintf "invalid order: %s" s

let rec private applyOrder market order = 
    match order.orderType with
    | BuildDepartment newDepartment ->
        let newBuildings =
            market.buildings |> List.map (fun ob -> 
                if ob = order.target then { ob with departments = newDepartment::ob.departments }
                else ob)
        { market with buildings = newBuildings }
    | DownsizeDepartment department ->
        let newBuildings =
            market.buildings |> List.map (fun ob -> 
                if ob = order.target then { ob with departments = List.except [department] ob.departments }
                else ob)
        { market with buildings = newBuildings }
    | MoveExecutive executive ->
        let currentBuilding = market.buildings |> List.find (fun b -> 
            List.contains (TopFloor (Some executive)) b.departments)
        let withoutTopFloor = List.except [(TopFloor (Some executive))] currentBuilding.departments
        let newCurrentBuilding = { currentBuilding with departments = (TopFloor None)::withoutTopFloor }
        let withoutCurrentBuilding = List.except [currentBuilding] market.buildings
        let newMarket = { market with buildings = newCurrentBuilding::withoutCurrentBuilding }
        applyOrder newMarket { order with orderType = BuildDepartment (TopFloor (Some executive)) }

let processOrders executive market = 
    executive.orders |> List.fold applyOrder market