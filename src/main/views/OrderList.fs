module Main.Views.OrderList

open Xelmish.UI
open Constants
open Main.Model
open Main.Orders
open Main.Update

let defaultMargin = margin (px defaultMargin)

let contentFor (model: MainModel) activeCategory dispatch = 
    let corp = model.market.player
    let orders = validOrdersFor corp 

    let headings = 
        orders 
        |> List.map (fun (category, categoryOrders) ->
            let canBeSelected = category <> activeCategory && List.exists snd categoryOrders
            let backgroundColour = if category = activeCategory then colours.buttonPressed else colours.button
            button [ 
                defaultMargin
                enabled canBeSelected
                buttonDisabledColour backgroundColour
                onclick (fun () -> dispatch (SelectOrderCategory category)) 
                ] category)

    let orderButtons = 
        orders 
        |> List.pick (fun (category, categoryOrders) -> if category = activeCategory then Some categoryOrders else None)
        |> List.chunkBySize 5 
        |> Seq.map (fun orders ->
                let buttons = 
                    orders 
                    |> List.map (fun (order, possible) -> 
                        button [ 
                            defaultMargin
                            enabled possible
                            onclick (fun () -> dispatch (SelectOrder order)) 
                            ] order.displayName)
                let padding = 5 - orders.Length
                if padding = 0 then buttons
                else buttons @ List.replicate padding (row [] []))
        |> Seq.toArray
    [    
        col [ padding (px 5); fontSize 14. ] [
            row [ height (pct 0.15) ] headings
            row [ ] [
                col [ width (pct 0.225) ] orderButtons.[0]
                col [ width (pct 0.225) ] (if orderButtons.Length > 1 then orderButtons.[1] else [])
                col [ width (pct 0.225) ] (if orderButtons.Length > 2 then orderButtons.[2] else [])
                col [ width (pct 0.225) ] (if orderButtons.Length > 3 then orderButtons.[3] else [])
                col [] [
                    row [ height (pct 0.8) ] []
                    button [ defaultMargin; onclick (fun () -> dispatch Cancel) ] "X"
                ]
            ]
        ]
    ]