module Main.View

open Xelmish.Viewables
open Xelmish.Model
open Constants
open View_Market
open View_Interface

let view model dispatch =
    [   yield! renderMarket model dispatch
        yield! renderUserInterface model dispatch
        
        if quitOnEscape then
            yield onkeydown Keys.Escape exit ]