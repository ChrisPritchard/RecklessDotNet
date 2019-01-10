module Reckless.Iso

open Constants

let tw, th = Constants.tileSize
let rx, ry = Constants.windowSize

let cx, cy = (rx - (tw * 10)) / 2 + (tw/2), ry / 2 + (th/2)

let iso x y = 
    cx + (x * tw/2 + y*tw/2), cy + (y*th/2 - x*th/2)

let bottomCentre x y w h =
    x - (w/2), y - h

let isoRect x y w h = 
    let ix, iy = iso x y
    let ox, oy = bottomCentre ix iy w h
    ox, oy, w, h