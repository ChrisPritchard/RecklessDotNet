module Common

let windowSize = 1024, 768
let winw, winh = windowSize
let tileWidth, tileHeight = 64, 32
let maxMapSize = 10

let turnTransitionTime = 500.

let productIncome = 100
let ordersPerTurn = 2

//let rec orderCost =
//    function
//    | BuildDepartment (_, Product _) -> 0
//    | BuildDepartment (_, Marketing) -> 2500
//    | BuildDepartment (_, Research) -> 2500
//    | BuildDepartment (_, Acquisitions) -> 3000
//    | BuildExtension (_, QA) -> 500
//    | ResearchIdea _ -> 1000
//    | BuildOffice (_, _, _, dep) -> 
//        7500 + 
//        orderCost (BuildDepartment (Unchecked.defaultof<Office>, dep))

//let departmentCost = 
//    function
//    | Research -> 500
//    | Marketing -> 500
//    | Admin _ -> 500
//    | Product _ -> 0