module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ViewOrders
    | SelectOrderCategory of string
    | SelectOrder of Order
    | ConfirmOrderTargetTile
    | CancelOrderTargetTile
    | Cancel

let update message model = 
    match message, model.playerAction with
    | SelectTile (x, y), Overview when model.market.tiles.Contains (x, y) -> 
        { model with selectedTile = Some (x, y) }, Cmd.none

    | ViewOrders, Overview -> 
        { model with playerAction = OrderTypeSelect defaultOrderCategory }, Cmd.none
    | SelectOrderCategory category, OrderTypeSelect _ -> 
        { model with playerAction = OrderTypeSelect category }, Cmd.none
    | Cancel, OrderTypeSelect _ -> 
        { model with playerAction = Overview }, Cmd.none

    | SelectOrder order, OrderTypeSelect _ -> 
        let targets = { corp = order.corpTransform model.market.player; ownOffice = None; otherOffice = None }
        { model with 
            selectedTile = None
            playerAction = TargetOrder (order, targets, 0) }, Cmd.none
    | SelectTile (x, y), TargetOrder (order, _, componentIndex) 
        when model.market.tiles.Contains (x, y) && order.components.[componentIndex].validate (x, y) model.market  -> 
        { model with selectedTile = Some (x, y) }, Cmd.none
    | ConfirmOrderTargetTile, TargetOrder (order, targets, componentIndex) when model.selectedTile <> None ->
        let newTargets = updateTargets order.components.[componentIndex] model.selectedTile targets
        if componentIndex = order.components.Length - 1 then
            { model with 
                market = applyOrder order newTargets
                playerAction = Overview
                selectedTile = Some model.market.player.headOffice.pos }, Cmd.none
        else
            { model with 
                selectedTile = None
                playerAction = TargetOrder (order, newTargets, componentIndex + 1) }, Cmd.none
    | CancelOrderTargetTile, TargetOrder _ when model.selectedTile <> None ->
        { model with selectedTile = None }, Cmd.none
    | Cancel, TargetOrder _ -> 
        { model with 
            playerAction = Overview
            selectedTile = Some model.market.player.headOffice.pos }, Cmd.none

    | _ -> model, Cmd.none