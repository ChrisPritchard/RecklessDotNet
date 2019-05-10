module SharedModel

open Xelmish.Model

type Executive = {
    name: string
}

type Corporation = {
    name: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    colour: Colour
}
and Office = {
    x: int
    y: int
    managedOffices: Office list
    departments: Department list
}
and Department =
    | Product of quality: int 
    | Marketing
    | Research
    | Admin of Executive option

type Market = {
    market: Set<int * int>
    player: Corporation
    others: Corporation list
}