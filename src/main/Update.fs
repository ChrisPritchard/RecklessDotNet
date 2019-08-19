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
    match message, model.currentInterface with
    | SelectTile (x, y), Information _ when model.market.tiles.Contains (x, y) -> 
        { model with currentInterface = Information (x, y) }, Cmd.none
    | ViewOrders, Information _ -> 
        { model with currentInterface = OrderTypeSelect defaultOrderCategory }, Cmd.none
    | SelectOrderCategory category, OrderTypeSelect _ -> 
        { model with currentInterface = OrderTypeSelect category }, Cmd.none
    | SelectOrder order, OrderTypeSelect _ -> 
        { model with currentInterface = TargetOrder order }, Cmd.none
    | Cancel, OrderTypeSelect _ -> 
        { model with currentInterface = Information model.market.player.headOffice.pos }, Cmd.none
    | _ -> model, Cmd.none