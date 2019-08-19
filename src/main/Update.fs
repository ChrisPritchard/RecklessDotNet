module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ViewOrders
    | SelectOrder of Order

let update message model = 
    match message, model.currentInterface with
    | SelectTile (x, y), Information _ when model.market.tiles.Contains (x, y) -> 
        { model with currentInterface = Information (x, y) }, Cmd.none
    | ViewOrders, Information _ -> 
        { model with currentInterface = OrderTypeSelect }, Cmd.none
    | _ -> model, Cmd.none