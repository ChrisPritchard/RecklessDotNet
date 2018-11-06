module Model

type Player = {
    name: string
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
}
and Department = 
    | Product of quality: int 
    | Marketing of used: bool 
    | Research of used: bool 
    | Admin