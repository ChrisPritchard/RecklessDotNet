module Model

open Microsoft.Xna.Framework

type Player = {
    name: string
    //colour: Color
    cash: int
    ideas: int
}

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
    quality: int
}
and Department = Product | Marketing | Research | TopFloor

type Executive = {
    name: string
    building: Building
    level: ExecutiveLevel
    orders: Order list
}
and ExecutiveLevel = | Junior = 1 | Intermediate = 2 | Senior = 3
and Order = 
    | BuildDepartment of Department * Building 
    | DownsizeDepartment of Building * Department
    | CreateIdea of Department