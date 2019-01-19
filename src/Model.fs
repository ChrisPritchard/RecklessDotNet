module Reckless.Model

open Microsoft.Xna.Framework
open GameCore.GameModel

type Corporation = {
    name: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    orders: Order list
    colour: Color
}
and Office = {
    x: int
    y: int
    managedOffices: Office list
    departments: Department list
    extensions: Extension list
}
and Department =
    | Product of quality: int 
    | Marketing
    | Research
    | Acquisitions
and Extension =
    | QA
and Order =
    | BuildDepartment of Office * Department
    | BuildExtension of Office * Extension
    | ResearchIdea of researchLocation:Office
    | BuildOffice of acquisitions:Office * x:int * y:int * Department

type GameState = {
    market: Set<int * int>
    corps: Corporation list
    productTiles: Map<int * int, (Corporation * int) list>
    selectedTile: (int * int) option
}

let rec allOffices office = [
    yield office
    yield! List.collect allOffices office.managedOffices
]

let productTiles office = 
    let products = office.departments |> List.sumBy (function Product _ -> 1 | _ -> 0)
    [-products..products] |> List.collect (fun x ->
    [-products..products] |> List.map (fun y -> x, y))
    |> List.filter (fun (x, y) -> abs x + abs y <= products)
    |> List.map (fun (x, y) -> office.x + x, office.y + y)

let rec allProductTiles parent office = 
    let quality, localMarketing = 
        ((0, 0), office.departments) 
        ||> List.fold (fun (q, m) -> 
            function
            | Product v -> q + v, m
            | Marketing -> q, m + 1
            | _ -> q, m)
    let parentMarketing = 
        match parent with 
        | Some o -> o.departments |> List.sumBy (function Marketing -> 1 | _ -> 0) 
        | _ -> 0
    let quality = quality * pown 2 (localMarketing + parentMarketing)
    [
        yield! productTiles office |> List.map (fun (x, y) -> x, y, quality)
        yield! List.collect (allProductTiles (Some office)) office.managedOffices
    ]

let rec updateQuality office researchOffices =
    let hasResearch = List.contains Research office.departments && not <| List.contains office researchOffices
    let qaCount = office.extensions |> List.sumBy (fun e -> match e with QA -> 1)
    let newDepartments = 
        office.departments
        |> List.map (function
        | Product q -> 
            let degraded = if hasResearch then q else q - 10
            let enhanced = degraded + (qaCount * 5)
            Product (max 10 enhanced)
        | d -> d)
    let newManaged = office.managedOffices |> List.map (fun o -> updateQuality o researchOffices)
    { office with departments = newDepartments; managedOffices = newManaged }