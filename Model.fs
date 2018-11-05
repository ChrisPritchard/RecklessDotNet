module Model

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
and Department = Product of quality: int | Marketing | Research of QA: bool | Executive of Executive
and Executive = {
    name: string
    owner: Player
    level: ExecutiveLevel
    orders: Order list
}
and ExecutiveLevel = | Junior = 1 | Intermediate = 2 | Senior = 3
and Order = { target: Building; orderType: OrderType }
and OrderType = BuildDepartment of Department

let issueOrder order executive = 
    if executive.orders.Length > int executive.level then Error "too many orders"
    elif order.target.owner <> executive.owner then Error "un-owned target"
    // TODO: validate order
    else Ok { executive with orders = order::executive.orders }