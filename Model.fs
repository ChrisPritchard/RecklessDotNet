module Model

type Player = {
    name: string
    cash: int
    ideas: int
    buildings: Building list
}
and Building = {
    x: int
    y: int
    departments: Department list
}
and Department = 
    | Product of quality: int 
    | Marketing of used: bool 
    | Research of used: bool 
    | Acquisitions of used: bool 

let advanceBuilding building =
    let hasResearch = 
        building.departments 
        |> Seq.tryFind (function | Research _ -> true | _ -> false)
        |> function | Some _ -> true | _ -> false
    let newDepartments = 
        building.departments 
        |> List.map (function
        | Product p when not hasResearch -> Product (max (p - 10) 10)
        | d -> d)
    { building with departments = newDepartments }