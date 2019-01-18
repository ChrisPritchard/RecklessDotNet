module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Iso

let private getCursor runState =
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))

let getView runState gameState = 
    [
        // render map and corps
        // render popups (office info, tile info)
        // render ui commands (orders, end turn)
        // render status (cash, cashflow, ideas)
        // render mouse cursor

        let mousePos = mouseTile runState |> Option.defaultValue (-1, -1)
        yield! Map.render mousePos gameState
        yield! Interface.render gameState
        yield 99, getCursor runState
    ] 
    |> List.sortBy fst 
    |> List.map snd 