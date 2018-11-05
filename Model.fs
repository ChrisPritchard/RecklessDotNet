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
and Department = Product of quality: int | Marketing | Research of QA: bool | TopFloor of Executive option
and Executive = {
    name: string
    owner: Player
    level: ExecutiveLevel
    orders: Order list
}
and ExecutiveLevel = | Junior = 1 | Intermediate = 2 | Senior = 3
and Order = { target: Building; orderType: OrderType }
and OrderType = BuildDepartment of Department | DownsizeDepartment of Department | MoveExecutive of Executive