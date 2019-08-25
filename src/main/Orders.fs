module Main.Orders

open Model

let buildOrders = 
    [ 
        "Build New Product", 0, 1, Product 100, "Product"
        "Build New Marketing", 2500, 0, Marketing, "Marketing"
        "Build New R & D", 2500, 0, Research, "Research"
        "Build New Admin", 2500, 0, Admin None, "Admin"
        "Build New HR", 5000, 0, HR, "Human Resources"  // TODO: source real values
        "Build New Acquisitions", 5000, 0, Acquisitions, "Acquisitions"  // TODO: source real values
        "Build New Legal", 5000, 0, Legal, "Legal"  // TODO: source real values
        "Build New Security", 5000, 0, Security, "Security"  // TODO: source real values
        "Build New Computer Core", 5000, 0, ComputerCore, "Computer Core"  // TODO: source real values
    ] |> List.map (fun (display, baseCost, ideaCost, dep, name) ->
        let order = {
            displayName = display
            baseCost = baseCost
            ideaCost = ideaCost
            }
        let conditions = [
            OfficeCondition (
                sprintf "Select one of your Buildings to receive the new %s department" name,
                (fun office isOwn -> isOwn && office.departments.Length < 6), 
                fun office -> { office with departments = dep::office.departments })
            ]
        order, conditions)

let buildBuildingOrder = {
    displayName = "Build New Building"
    baseCost = 7500
    ideaCost = 0 
    }

let downSizeOrder = {
    displayName = "Downsize Department"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    }

let transferOrder = {
    displayName = "Transfer Department"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    }

let researchIdeaOrder = {
    displayName = "Research Idea"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    }

let ordersByCategory = 
    [
        "Corporate", [yield! Seq.map fst buildOrders; yield buildBuildingOrder; yield downSizeOrder; yield transferOrder]
        "R & D", [researchIdeaOrder]
    ]

let orderConditions = 
    (buildBuildingOrder, [])::(downSizeOrder, [])::(transferOrder, [])::(researchIdeaOrder, [])::buildOrders
    |> Map.ofList