module Main.View

open Xelmish.Model
open Xelmish.Viewables
open Constants
open View_Market
open Update

let view model dispatch =
    [   yield setSmoothSampling ()
        yield! renderMarket model dispatch

        if model.newInterfaceMode then
            yield! View_Interface2.renderUserInterface model dispatch
        else
            yield! View_Interface.renderUserInterface model dispatch
        
        yield onkeyup Keys.Space (fun () -> dispatch ChangeInterfaceMode)

        if quitOnEscape then
            yield onkeydown Keys.Escape exit ]