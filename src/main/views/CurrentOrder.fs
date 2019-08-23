module Main.Views.CurrentOrder

open Xelmish.UI
open Constants
open Main.Model
open Main.Update

let contentFor (model: MainModel) order componentIndex dispatch = 
    let corp = model.market.player
    let selected = model.selectedTile |> Option.bind model.market.atTile

    [    
        col [ width (pct 0.6) ] [
            yield text [ fontSize 25.; height (pct 0.18); defaultPadding ] order.displayName
            yield row [] []
        ]
        InfoPanels.selectedInfo selected
        col [ padding (px 10); ] [
            row [ height (pct 0.8) ] []
            button [ defaultMargin; onclick (fun () -> dispatch Cancel) ] "X"
        ]
    ]