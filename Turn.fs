module Turn

open Model

let productTiles office parentMarketing = 
    let products, quality, localMarketing = 
        office.departments 
        |> List.fold (fun (p, q, m) -> 
            function
            | Product v -> p + 1, q + v, m
            | Marketing -> p, q, m + 1
            | _ -> p, q, m) (0, 0, 0)
    let quality = quality * pown 2 (localMarketing + parentMarketing)
    
    [-products..products] |> List.collect (fun x ->
    [-products..products] |> List.map (fun y -> x, y))
    |> List.filter (fun (x, y) -> abs x + abs y <= products)
    |> List.map (fun (x, y) -> office.x + x, office.y + y, quality)
    
let updateQuality office researchInUse =
    let hasResearch = List.contains Research office.departments
    let qaCount = office.extensions |> List.sumBy (fun e -> match e with QA -> 1)
    let newDepartments = 
        office.departments
        |> List.map (function
        | Product q -> 
            let degraded = if hasResearch && not researchInUse then q else q - 10
            let enhanced = degraded + (qaCount * 5)
            Product (max 10 enhanced)
        | d -> d)
    { office with departments = newDepartments }