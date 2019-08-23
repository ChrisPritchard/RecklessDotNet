module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ViewOrders
    | SelectOrderCategory of string
    | SelectOrder of Order
    | Cancel

let update message model = 
    match message, model.playerAction with
    | SelectTile (x, y), Overview when model.market.tiles.Contains (x, y) -> 
        { model with selectedTile = (x, y) }, Cmd.none
    | ViewOrders, Overview -> 
        { model with playerAction = OrderTypeSelect defaultOrderCategory }, Cmd.none
    | SelectOrderCategory category, OrderTypeSelect _ -> 
        { model with playerAction = OrderTypeSelect category }, Cmd.none
    | SelectOrder order, OrderTypeSelect _ -> 
        let targets = { corp = order.corpTransform model.market.player; ownOffice = None; otherOffice = None }
        { model with playerAction = TargetOrder (order, targets, order.components.[0]) }, Cmd.none
    | Cancel, OrderTypeSelect _ -> 
        { model with playerAction = Overview }, Cmd.none
    | _ -> model, Cmd.none