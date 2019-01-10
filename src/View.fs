module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Iso

let tw, th = Constants.tileSize

let getView runState (map, (corps: Corporation list)) = [
    yield! 
        Set.toList map 
        |> List.map (fun (x, y) -> 
            Image ("tile", isoRect x y tw th, Color.White))
    yield! 
        corps |> List.map (fun corp -> 
            let o = corp.headOffice
            Image ("office", isoRect o.x o.y tw (th*3), corp.colour))

    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    yield Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow)) ]