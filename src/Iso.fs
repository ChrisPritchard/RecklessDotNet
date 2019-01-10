module Reckless.Iso

open GameCore.GameModel
open Constants
open Reckless

let tw, th = tileSize
let rx, ry = windowSize

let cx, cy = (rx - (tw * Constants.mapSize)) / 2 + (tw/2), ry / 2 + (th/2)

let iso x y = 
    cx + (x * tw/2 + y*tw/2), cy + (y*th/2 - x*th/2)

let bottomCentre x y w h =
    x - (w/2), y - h

let isoRect x y w h = 
    let ix, iy = iso x y
    let ox, oy = bottomCentre ix iy w h
    ox, oy, w, h

let mouseTile (runState: RunState) = 
    let mx, my = runState.mouse.position
    let mx, my = mx - cx, my - cy

    let tx = mx/tw
    let ty = my/th - mx*th

    let map = Constants.mapSize
    if tx >= 0 && tx < map && ty >= 0 && ty < map then
        Some (tx, ty)
    else 
        None
