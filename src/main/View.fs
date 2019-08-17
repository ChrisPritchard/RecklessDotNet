module Main.View

open Xelmish.Model
open Xelmish.Viewables
open Constants
open View_Market
open View_Interface

let view (model: Model.MainModel) dispatch =
    [   yield setSmoothSampling ()
        yield! renderMarket model dispatch
        yield! renderUserInterface model dispatch

        if quitOnEscape then
            yield onkeydown Keys.Escape exit ]