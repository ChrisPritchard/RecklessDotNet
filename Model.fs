module Model

open Constants
open Microsoft.Xna.Framework

type Player = Player of name: string * colour: Color

type Market = {
    width: int
    height: int
    buildings: Building list
}
and Building = {
    owner: Player
    x: int
    y: int
    departments: Department list
}
and Department = Product of quality: int | Marketing | Research of QA: bool | Executive of Executive option
and Executive = {
    name: string
    owner: Player
    level: ExecutiveLevel
    orders: Order list
}
and ExecutiveLevel = | Junior = 1 | Intermediate = 2 | Senior = 3
and Order = { target: Building; orderType: OrderType }
and OrderType = BuildDepartment of Department | MoveExecutive

let validateOrder orderType target = 
    match orderType with
    | BuildDepartment _ -> 
        if target.departments.Length = maxDepartments then Error "max departments reached"
        else Ok ()
    | MoveExecutive ->
        match List.tryFind (function (Executive None) -> true | _ -> false) target.departments with
        | Some _ -> Ok ()
        | _ -> Error "no empty executive department"

let issueOrder order executive = 
    if executive.orders.Length > int executive.level then Error "too many orders"
    elif order.target.owner <> executive.owner then Error "un-owned target"
    else
        match validateOrder order.orderType order.target with
        | Ok () -> Ok { executive with orders = order::executive.orders }
        | Error s -> Error <| sprintf "invalid order: %s" s