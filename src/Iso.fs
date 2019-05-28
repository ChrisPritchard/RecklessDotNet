module Iso

open Constants

let private rx, ry = windowSize
let private centreX, centreY = (rx - (tileWidth * Constants.maxMapSize)) / 2 + (tileWidth/2), ry / 2 + (tileHeight/2)

let private iso x y = 
    centreX + (x * tileWidth/2 + y*tileWidth/2), centreY + (y*tileHeight/2 - x*tileHeight/2)

let private bottomCentre x y w h =
    x - (w/2), y - h

let isoRect x y w h = 
    let ix, iy = iso x y
    let ox, oy = bottomCentre ix iy w h
    ox, oy, w, h

let mouseTile (mousePosition: int * int) = 
    let mx, my = mousePosition
    // offset to where the tiles have been rendered from
    let mx, my = float (mx - centreX), float (my - centreY)
    // convert tilesize to float (so ceil/floor work)
    let ftw, fth = float tileWidth, float tileHeight

    // calculate tile. this works when tiles are rendered from 
    // left point (e.g. +x is up-right, +y is down-right)
    (floor >> int) ((-my / fth) + (mx / ftw)),
    (ceil >> int) ((mx / ftw) + (my / fth))