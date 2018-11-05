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
    orders: Order list
}
and Order = BuildDeparment of Building * Department

