module Main

open Elmish

type Model = 
    {   market: Market.Model    }

let init () = { market = Market.init () }

type Message = MarketMessage of Market.Message

let update message model = 
    match message with 
    | MarketMessage message -> 
        let updatedMarket, command = Market.update message model.market
        { model with market = updatedMarket }, Cmd.map MarketMessage command

let view model dispatch =
    Market.view model.market (MarketMessage >> dispatch)