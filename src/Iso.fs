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
    // offset to where the tiles have been rendered from
    let mx, my = float (mx - cx), float (my - cy)
    // convert tilesize to float (so ceil/floor work)
    let ftw, fth = float tw, float th

    // calculate tile. this works when tiles are rendered from 
    // left point (e.g. +x is up-right, +y is down-right)
    let tx, ty = 
        (floor >> int) ((-my / fth) + (mx / ftw)),
        (ceil >> int) ((mx / ftw) + (my / fth))

    let map = Constants.mapSize
    if tx >= 0 && tx < map && ty >= 0 && ty < map then
        Some (tx, ty)
    else 
        None