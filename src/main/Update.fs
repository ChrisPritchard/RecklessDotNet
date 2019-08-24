module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ViewOrders of Executive
    | SelectOrderCategory of string
    | SelectOrder of Order
    | ConfirmOrderTargetTile
    | CancelOrderTargetTile
    | Cancel

let private validateTileFor orderComponent tile (market: Market) =
    match orderComponent, market.atTile tile with
    | OfficeTransform (_, checkOffice, _), Some (OfficeInfo ox) ->
        checkOffice ox.office (ox.corporation = market.player)
    | _ -> false

let private updateTargets orderComponent tile targets (market: Market) =
    match orderComponent, tile |> Option.bind market.atTile with
    | OfficeTransform (_, _, transform), Some (OfficeInfo ox) ->
        if ox.corporation = market.player && targets.ownOffice = None then
            { targets with ownOffice = Some (transform ox.office) }
        else
            { targets with otherOffice = Some (transform ox.office) }
    | _ -> failwith "invalid component for selected tile"
    
let defaultOrderCategory = "Corporate"

let update message model = 
    match message, model.playerAction with
    | SelectTile (x, y), Overview when model.market.tiles.Contains (x, y) -> 
        { model with selectedTile = Some (x, y) }, Cmd.none

    | ViewOrders executive, Overview -> 
        { model with playerAction = OrderTypeSelect (executive, defaultOrderCategory) }, Cmd.none
    | SelectOrderCategory category, OrderTypeSelect (executive, _) -> 
        { model with playerAction = OrderTypeSelect (executive, category) }, Cmd.none
    | Cancel, OrderTypeSelect _ -> 
        { model with playerAction = Overview }, Cmd.none

    | SelectOrder order, OrderTypeSelect (executive, _) -> 
        let targets = { ownOffice = None; otherOffice = None }
        let orderState = { 
            executive = executive
            order = order
            targets = targets
            componentIndex = 0 }
        { model with 
            selectedTile = None
            playerAction = TargetOrder orderState }, Cmd.none
    | SelectTile (x, y), TargetOrder orderState 
        when model.market.tiles.Contains (x, y) && validateTileFor orderState.orderComponent (x, y) model.market  -> 
        { model with selectedTile = Some (x, y) }, Cmd.none
    | ConfirmOrderTargetTile, TargetOrder orderState when model.selectedTile <> None ->
        let newTargets = updateTargets orderState.orderComponent model.selectedTile orderState.targets model.market
        if orderState.componentIndex = orderState.order.components.Length - 1 then
            let confirmedOrder = { 
                corporation = model.market.player
                executive = orderState.executive
                order = orderState.order
                targets = newTargets }
            { model with 
                turnOrders = confirmedOrder::model.turnOrders
                playerAction = Overview
                selectedTile = Some model.market.player.headOffice.pos }, Cmd.none
        else
            { model with 
                selectedTile = None
                playerAction = TargetOrder { orderState with componentIndex = orderState.componentIndex + 1 } }, Cmd.none
    | CancelOrderTargetTile, TargetOrder _ when model.selectedTile <> None ->
        { model with selectedTile = None }, Cmd.none
    | Cancel, TargetOrder _ -> 
        { model with 
            playerAction = Overview
            selectedTile = Some model.market.player.headOffice.pos }, Cmd.none

    | _ -> model, Cmd.none