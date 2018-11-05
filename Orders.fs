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

let private applyOrder market order = 
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
        let department = TopFloor (Some executive)
        let newBuildings = market.buildings |> List.map (fun b -> 
            if List.contains department b.departments then 
                let newDepartments = b.departments |> List.map (fun d -> if d = department then TopFloor None else d)
                { b with departments = newDepartments } 
            elif b = order.target then { b with departments = department::b.departments } 
            else b)
        { market with buildings = newBuildings }

let processOrders executive market = 
    executive.orders |> List.fold applyOrder market