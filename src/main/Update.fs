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

let private validateTileFor orderComponent tile (market: Market) =
    match orderComponent, market.atTile tile with
    | OfficeTransform (checkOffice, _), Some (OfficeInfo ox) ->
        checkOffice ox.office (ox.corporation = market.player)
    | _ -> false

let private updateTargets orderComponent tile targets (market: Market) =
    match orderComponent, tile |> Option.bind market.atTile with
    | OfficeTransform (_, transform), Some (OfficeInfo ox) ->
        if ox.corporation = market.player && targets.ownOffice = None then
            { targets with ownOffice = Some (transform ox.office) }
        else
            { targets with otherOffice = Some (transform ox.office) }
    | _ -> failwith "invalid component for selected tile"

let private applyOrder order targets (market: Market) =
    market
    //{ market with 
    //    player = {
    //        market.player with
    //            orders = (order.displayName, targets)::market.player.orders 
    //            // TODO: how to fold targets over player offices for order selection or transformation?
    //    } }

let defaultOrderCategory = "Corporate"

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
        when model.market.tiles.Contains (x, y) && validateTileFor order.components.[componentIndex] (x, y) model.market  -> 
        { model with selectedTile = Some (x, y) }, Cmd.none
    | ConfirmOrderTargetTile, TargetOrder (order, targets, componentIndex) when model.selectedTile <> None ->
        let orderComponent = order.components.[componentIndex]
        let newTargets = updateTargets orderComponent model.selectedTile targets model.market
        if componentIndex = order.components.Length - 1 then
            { model with 
                market = applyOrder order newTargets model.market
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