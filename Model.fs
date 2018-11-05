module Model

type Market = {
    width: int
    height: int
    buildings: Building list
}
and Building = {
    owner: string
    x: int
    y: int
    departments: Department list
}
and Department = Product of quality: int | Marketing | Research of QA: bool