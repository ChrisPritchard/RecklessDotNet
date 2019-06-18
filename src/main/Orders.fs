module Main.Orders

open Model

type Order = 
    {   displayName: string
        category: string
        components: OrderComponent list }
and OrderComponent =
    | CorpTransform of condition:(Corporation -> bool) * action:(Corporation -> Corporation)
    | OfficeTransform of condition:(Office -> bool -> bool) * action:(Office -> Office)

let buildProductOrder = {
    displayName = "Build New Product"
    category = "Corporate"
    components = [
        CorpTransform (
            (fun corp -> corp.ideas > 0), 
            fun corp -> { corp with ideas = corp.ideas - 1 })
        OfficeTransform (
            (fun office isOwn -> isOwn && office.departments.Length < 6), 
            fun office -> { office with departments = Product 100::office.departments })
    ]
}

let researchIdeaOrder = {
    displayName = "Research Idea"
    category = "R & D"
    components = [
        CorpTransform (
            (fun corp -> corp.cash >= 1000), 
            fun corp -> { corp with 
                            cash = corp.cash - 1000
                            ideas = corp.ideas + 1 })
        OfficeTransform (
            (fun office isOwn -> isOwn && List.contains Research office.departments), 
            fun office -> office) // TODO: office with used = research?
    ]
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

let allOrders = [buildProductOrder; researchIdeaOrder]

let validOrdersFor corp =
    allOrders 
    |> List.filter (fun order ->
        order.components 
        |> List.forall (function
            | CorpTransform (checkCorp, _) -> checkCorp corp
            | OfficeTransform (checkOffice, _) -> 
                corp.allOffices 
                |> List.exists (fun (office, _, _) -> checkOffice office true)))
