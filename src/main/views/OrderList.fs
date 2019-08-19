module Main.Views.OrderList

open Xelmish.UI
open Constants
open Main.Model
open Main.Orders
open Main.Update

let defaultMargin = margin (px defaultMargin)

let contentFor (model: MainModel) dispatch = 
    let corp = model.market.player
    let orders = validOrdersFor corp 
    let buttons = 
        orders
        |> List.chunkBySize 5 
        |> Seq.map (fun orders ->
            let buttons = orders |> List.map (fun order -> 
                button [ defaultMargin; onclick (fun () -> dispatch (SelectOrder order)) ] order.displayName)
            let padding = 5 - orders.Length
            if padding = 0 then buttons
            else buttons @ List.replicate padding (row [] []))
        |> Seq.toArray
    [       
        col [ padding (px 10); width (pct 0.225) ] buttons.[0]
        col [ padding (px 10); width (pct 0.225) ] (if buttons.Length > 1 then buttons.[1] else [])
        col [ padding (px 10); width (pct 0.225) ] (if buttons.Length > 2 then buttons.[2] else [])
        col [ padding (px 10); width (pct 0.225) ] (if buttons.Length > 3 then buttons.[3] else [])
        col [ padding (px 10); ] [
            row [ height (pct 0.8) ] []
            button [ defaultMargin; onclick (fun () -> dispatch Cancel) ] "X"
        ]
    ]