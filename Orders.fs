module Orders

open Model
open Constants

let validateOrder orderType target = 
    match orderType with
    | BuildDepartment _ -> 
        if target.departments.Length = maxDepartments then Error "max departments reached"
        else Ok ()
    | MoveExecutive ->
        if List.contains (TopFloor None) target.departments 
        then Ok () else Error "no empty executive department"

let issueOrder order executive = 
    if executive.orders.Length > int executive.level then Error "too many orders"
    elif order.target.owner <> executive.owner then Error "un-owned target"
    else
        match validateOrder order.orderType order.target with
        | Ok () -> Ok { executive with orders = order::executive.orders }
        | Error s -> Error <| sprintf "invalid order: %s" s

let processOrders executive market = 
    executive.orders |> List.fold (fun marketDelta o -> 
        match o.orderType with
        | BuildDepartment newDepartment -> 
            let newBuildings =
                marketDelta.buildings |> List.map (fun ob -> 
                    if ob = o.target then { ob with departments = newDepartment::ob.departments }
                    else ob)
            { marketDelta with buildings = newBuildings }
        )
        market