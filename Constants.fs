module Constants

open Model

let rec orderCost =
    function
    | BuildDepartment (Product _) -> 0
    | BuildDepartment Marketing -> 2500
    | BuildDepartment Research -> 2500
    | BuildDepartment Acquisitions -> 3000
    | BuildExtension QA -> 500
    | ResearchIdea -> 1000
    | BuildOffice (_, _, dep) -> 7500 + orderCost (BuildDepartment dep)

let departmentCost = 
    function
    | Research -> 500
    | Marketing -> 500
    | Acquisitions -> 500
    | Product _ -> 0