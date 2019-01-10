module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model

let tw, th = Constants.tileSize

let rx, ry = Constants.windowSize
let cx, cy = (rx - (tw * 10)) / 2 + (tw/2), ry / 2 + (th/2)

let iso x y = 
    cx + (x * tw/2 + y*tw/2), cy + (y*th/2 - x*th/2)

let bottomCentre x y w h =
    x - (w/2), y - h

let getView runState (map, (corps: Corporation list)) = [
    yield! 
        Set.toList map |> List.map (fun (x, y) -> 
            let ix, iy = iso x y
            let ix, iy = bottomCentre ix iy tw th
            let destRect = ix, iy, tw, th
            Image ("tile", destRect, Color.White))
    yield! 
        corps |> List.map (fun c -> 
            let ix, iy = iso c.headOffice.x c.headOffice.y
            let ix, iy = bottomCentre ix iy tw (th*3)
            let destRect = ix, iy, tw, th*3
            Image ("office", destRect, c.colour))

    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    yield Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow)) ]