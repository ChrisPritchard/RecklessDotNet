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

let radius building =
    building.departments 
    |> Seq.where (function Product _ -> true | _ -> false)
    |> Seq.length

let quality building =
    let baseQuality = 
        building.departments 
        |> Seq.sumBy (function | Product q -> q | _ -> 0)
    let marketingCount = 
        building.departments 
        |> Seq.where (function Marketing -> true | _ -> false) 
        |> Seq.length
    baseQuality * pown 2 marketingCount

let distance x y building =
    let core = (pown (x - building.x) 2) + (pown (y - building.y) 2)
    core |> float |> sqrt |> int

let owner x y market = 
    market.buildings 
    |> Seq.map (fun b -> b, distance x y b, quality b)
    |> Seq.filter (fun (b, dist, _) -> radius b >= dist)
    |> Seq.sortByDescending (fun (_, _, quality) -> quality)
    |> Seq.tryHead 
    |> Option.bind (fun (b, _, _) -> Some b.owner)