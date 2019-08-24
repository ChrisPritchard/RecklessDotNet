module Main.Orders

open Model

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

let buildProductOrder = {
    displayName = "Build New Product"
    baseCost = 0
    ideaCost = 1
    components = [|
        OfficeTransform (
            "Select one of your Buildings to receive the new Product department",
            (fun office isOwn -> isOwn && office.departments.Length < 6), 
            fun office -> { office with departments = Product 100::office.departments })
    |]
}

let otherDepBuildOrders = 
    [ 
        "Build New Marketing", 2500, Marketing, "Marketing"
        "Build New R & D", 2500, Research, "Research"
        "Build New Admin", 2500, Admin None, "Admin"
        "Build New HR", 5000, HR, "Human Resources"  // TODO: source real values
        "Build New Acquisitions", 5000, Acquisitions, "Acquisitions"  // TODO: source real values
        "Build New Legal", 5000, Legal, "Legal"  // TODO: source real values
        "Build New Security", 5000, Security, "Security"  // TODO: source real values
        "Build New Computer Core", 5000, ComputerCore, "Computer Core"  // TODO: source real values
    ] |> List.map (fun (display, cost, dep, name) ->
        {
            displayName = display
            baseCost = cost
            ideaCost = 0
            components = [|
                OfficeTransform (
                    sprintf "Select one of your Buildings to receive the new %s department" name,
                    (fun office isOwn -> isOwn && office.departments.Length < 6), 
                    fun office -> { office with departments = dep::office.departments })
            |]
        })

let buildBuildingOrder = {
    displayName = "Build New Building"
    baseCost = 7500
    ideaCost = 0 
    // TODO: add new office
    components = [|
        // tileselect
        // department select
        //OfficeTransform (
        //    (fun office isOwn -> isOwn && List.contains Research office.departments), 
        //    fun office -> office) // TODO: office with used = research?
    |]
}

let downSizeOrder = {
    displayName = "Downsize Department"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    components = [|
        OfficeTransform (
            "Select one of your Buildings to downsize a deparment in",
            (fun office isOwn -> isOwn && office.departments.Length > 0), 
            fun office -> { office with departments = office.departments })
        // select department
        // remove department
    |]
}

let transferOrder = {
    displayName = "Transfer Department"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    components = [|
        OfficeTransform (
            "Select one of your Buildings to transfer a deparment from",
            (fun office isOwn -> isOwn && office.departments.Length > 0), 
            fun office -> { office with departments = office.departments })
        // select department
        OfficeTransform (
            "Select one of your Buildings to transfer a deparment to",
            (fun office isOwn -> isOwn && office.departments.Length < 6), 
            fun office -> { office with departments = office.departments })
    |]
}

let researchIdeaOrder = {
    displayName = "Research Idea"
    baseCost = 1000 // TODO source real value
    ideaCost = 0
    // TODO: add idea
    components = [|
        OfficeTransform (
            "Select one of your Buildings containing a Research deparment",
            (fun office isOwn -> isOwn && List.contains Research office.departments), 
            fun office -> office) // TODO: office with used = research?
    |]
}

// Order process:
// for all orders, run through all components to check each can be applied at least once
// when an order is selected, progressively apply each component.
    // for corp transform apply automatically against current corp
    // for office transform, apply to all offices then offer player select on options (even if only one)
    // gather up results: new corp / new office

// multiple orders targeting same office:
    // once one order has been applied, a post-state office is created. new orders consider these post-states, and apply to them
    // as such the post-post-state becomes the new final state if all orders applied.
    
// process by which orders are applied:
    // user selects order
    // the ultimate target is a set of transitions with the order name:
        // orderName * (origCorp, newCorp) * (origOffice * newOffice) list
    // several update messages:
        // select order (order)
        // select office (orderName, corp transition, completed office transitions, remaining office transitions)
        // confirm order (as above, no remaining)
            // this last updates the market order list for the corp, for display or cancellation

let ordersByCategory = 
    [
        "Corporate", [yield buildProductOrder; yield! otherDepBuildOrders; yield buildBuildingOrder; yield downSizeOrder; yield transferOrder]
        "R & D", [researchIdeaOrder]
    ]