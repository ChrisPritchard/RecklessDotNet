﻿module Main.Views.CurrentOrder

open Xelmish.UI
open Constants
open Main.Model
open Main.Update

let wrapText (text: string) charsPerRow =
    (("", []), text.Split(' '))
    ||> Array.fold (fun (line, acc) word ->
        let newLength = line.Length + word.Length + 1
        if newLength > charsPerRow then "", line::acc
        else line + " " + word, acc)
    |> fun (line, acc) -> List.rev (line::acc) |> List.toArray

let contentFor (model: MainModel) order componentIndex dispatch = 
    let corp = model.market.player
    let selected = model.selectedTile |> Option.bind model.market.atTile

    [    
        col [ width (pct 0.6) ] [
            text [ fontSize 25.; height (pct 0.18); defaultPadding ] order.displayName
            row [ height (pct 0.5); defaultPadding ] [
                match order.components.[componentIndex] with
                | OfficeTransform (description, _, _) ->
                    yield paragraph [ defaultPadding ] (wrapText description 45)
            ]
            row [] [
                col [] [
                    text [ alignment 1. 0. ] "Base Cost: "
                    text [ alignment 1. 0. ] "Total Cost: "                    
                ]
                col [] [
                    text [] "TODO"//(string order.baseCost)
                    text [] "TODO"//(string (order.totalCost currentTargets))                    
                ]
                col [] [
                    text [ alignment 1. 0. ] "Cash: "
                    text [ alignment 1. 0. ] "Expenses: "
                ]
                col [] [
                    text [] (string corp.cash)
                    text [] "TODO"//(string corp.expenses)
                ]
            ]
        ]
        InfoPanels.selectedInfo selected
        col [ padding (px 10); ] [
            row [ height (pct 0.8) ] []
            button [ defaultMargin; onclick (fun () -> dispatch Cancel) ] "X"
        ]
    ]