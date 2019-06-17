﻿module Main.Orders

open Model

type Order = 
    {   name: string
        components: OrderComponent list }
and OrderComponent =
    | CorpTransform of condition:(Corporation -> bool) * action:(Corporation -> Corporation)
    | OfficeTransform of condition:(Office -> bool -> bool) * action:(Office -> Office)

let buildProductOrder = {
    name = "Build Product"
    components = [
        CorpTransform (
            (fun corp -> corp.ideas > 0), 
            fun corp -> { corp with ideas = corp.ideas - 1 })
        OfficeTransform (
            (fun office isOwn -> isOwn && office.departments.Length < 6), 
            fun office -> { office with departments = Product 100::office.departments })
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