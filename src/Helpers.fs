module Helpers

open Model

let rec allOffices office = office::List.collect allOffices office.managedOffices

let allCorps gameState = gameState.player::gameState.others

let officeProductTiles office = 
    let products = office.departments |> List.sumBy (function Product _ -> 1 | _ -> 0)
    [-products..products] |> List.collect (fun x ->
    [-products..products] |> List.map (fun y -> x, y))
    |> List.filter (fun (x, y) -> abs x + abs y <= products)
    |> List.map (fun (x, y) -> office.x + x, office.y + y)

let private corpProductTiles corp = 
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

let gameProductTiles gameState =
    allCorps gameState
    |> List.collect (fun c -> corpProductTiles c |> List.map (fun (x, y, q) -> (x, y), (c, q)))
    |> List.groupBy fst
    |> List.map (fun (pos, tiles) -> 
        let ordered = 
            tiles 
            |> List.sortByDescending (fun (_, (_, q)) -> q) 
            |> List.map snd
        pos, ordered)
    |> Map.ofList

let incomeByCorp productTiles productIncome =
    productTiles 
    |> Map.toList
    |> List.map (snd >> List.head >> fst)
    |> List.countBy id
    |> List.map (fun (c, n) -> c, n * productIncome)
    |> Map.ofList

let expensesForCorp (costs: Department -> int) corp = 
    allOffices corp.headOffice |> List.sumBy (fun o -> o.departments |> List.sumBy costs)

let expensesByCorp costs gameState = 
    allCorps gameState
    |> List.map (fun c -> c, expensesForCorp costs c)
    |> Map.ofList