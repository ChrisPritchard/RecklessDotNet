module Main.View

open Xelmish.Viewables
open Xelmish.Model
open Iso
open Model
open Orders
open Update
open View_Market

let renderUserInterface model dispatch = 
    // the user interface is in one of three modes:
    // - general info or default: shows the player's corp, their selected executive, and the selected entity's information
    // - order screen, giving order options
    // - order select screen, giving instructions on what to select (also shows the currently selected entity)
    [   yield onclickpoint (fun mousePos -> 
            let mouseTile = mouseTile mousePos
            if model.market.tiles.Contains mouseTile then 
                dispatch (SelectTile mouseTile)) ]

let view model dispatch =
    [   yield! renderMarket model
        yield! renderUserInterface model dispatch
        
        yield onkeydown Keys.Escape exit ]