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

let private validateTileFor orderState tile (market: Market) =
    match orderState.conditions, market.atTile tile with
    | OfficeCondition (_, checkOffice, _)::_, Some (OfficeInfo ox) ->
        checkOffice ox.office (ox.corporation = market.player)
    | _ -> false
    
let defaultOrderCategory = "Corporate"

let private initiateTargetOrder model order executive = 
    let orderState = { 
        executive = executive
        order = order
        conditions = orderConditions.[order]
        transforms = [] }
    { model with 
        selectedTile = None
        playerAction = TargetOrder orderState }
        
let private confirmOrderSelected model orderState =
    let newOrderState = 
        match orderState.conditions, Option.bind model.market.atTile model.selectedTile with
        | OfficeCondition (_, _, transform)::rest, Some (OfficeInfo ox) ->
            { orderState with
                conditions = rest
                transforms = OfficeTransform (ox.office, transform)::orderState.transforms }
        | _ -> failwith "invalid order state"
    match newOrderState.conditions with
    | [] ->
        { model with 
            turnOrders = (model.market.player, newOrderState)::model.turnOrders
            playerAction = Overview
            selectedTile = Some model.market.player.headOffice.pos }
    | _ ->
        { model with 
            playerAction = TargetOrder newOrderState
            selectedTile = None }

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

        initiateTargetOrder model order executive, Cmd.none

    | SelectTile (x, y), TargetOrder orderState when validateTileFor orderState (x, y) model.market -> 

        { model with selectedTile = Some (x, y) }, Cmd.none

    | ConfirmOrderTargetTile, TargetOrder orderState when model.selectedTile <> None ->

        confirmOrderSelected model orderState, Cmd.none

    | CancelOrderTargetTile, TargetOrder _ when model.selectedTile <> None ->

        { model with selectedTile = None }, Cmd.none

    | Cancel, TargetOrder _ -> 

        { model with 
            playerAction = Overview
            selectedTile = Some model.market.player.headOffice.pos }, Cmd.none

    | _ -> model, Cmd.none