module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Iso

let tw, th = Constants.tileSize

let getView runState (map, (corps: Corporation list)) = 
    [
    let mx, my = mouseTile runState |> Option.defaultValue (-1, -1)

    yield! 
        Set.toList map 
        |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th
            yield 0, Image ("tile", rect, Color.White)
            if (x, y) = (mx, my) then
                yield 1, Image ("tile-highlight", rect, Color.White)
            ])

    yield! 
        corps 
        |> List.collect (fun corp -> [
            let o = corp.headOffice
            let rect = isoRect o.x o.y tw (th*3)
            yield 2, Image ("office", rect, corp.colour)
            if (o.x, o.y) = (mx, my) then
                yield 3, Image ("office-highlight", rect, Color.White)
            ])
  
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    yield 5, Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))
    
    yield 4, Text ("font", sprintf "%i, %i" mx my, (10, 10, 100, 30), TopLeft, Color.Black) 
    ] |> List.sortBy fst |> List.map snd