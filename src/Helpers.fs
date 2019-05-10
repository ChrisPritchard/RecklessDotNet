module Helpers

open SharedModel

let rec allOffices office = office::List.collect allOffices office.managedOffices

let officeProductTiles office = 
    let products = office.departments |> List.sumBy (function Product _ -> 1 | _ -> 0)
    [-products..products] |> List.collect (fun x ->
    [-products..products] |> List.map (fun y -> x, y))
    |> List.filter (fun (x, y) -> abs x + abs y <= products)
    |> List.map (fun (x, y) -> office.x + x, office.y + y)

let corpProductTiles corp = 
    let rec gatherer parent office = 
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
            yield! officeProductTiles office |> List.map (fun (x, y) -> x, y, quality)
            yield! List.collect (gatherer (Some office)) office.managedOffices
        ]
    gatherer None corp.headOffice

//let incomeByCorp productTiles productIncome =
//    productTiles 
//    |> Map.toList
//    |> List.map (snd >> List.head >> fst)
//    |> List.countBy id
//    |> List.map (fun (c, n) -> c, n * productIncome)
//    |> Map.ofList

//let expensesForCorp (costs: Department -> int) corp = 
//    allOffices corp.headOffice |> List.sumBy (fun o -> o.departments |> List.sumBy costs)

//let rec updateQuality office researchOffices =
//    let hasResearch = List.contains Research office.departments && not <| List.contains office researchOffices
//    let qaCount = office.extensions |> List.sumBy (fun e -> match e with QA -> 1)
//    let newDepartments = 
//        office.departments
//        |> List.map (function
//        | Product q -> 
//            let degraded = if hasResearch then q else q - 10
//            let enhanced = degraded + (qaCount * 5)
//            Product (max 10 enhanced)
//        | d -> d)
//    let newManaged = office.managedOffices |> List.map (fun o -> updateQuality o researchOffices)
//    { office with departments = newDepartments; managedOffices = newManaged }