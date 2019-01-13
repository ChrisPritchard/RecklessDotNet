module Reckless.View

open Microsoft.Xna.Framework
open GameCore.GameModel
open Model
open Iso
open UI
open Turn

let tw, th = Constants.tileSize

let getView runState (map, (corps: Corporation list), ui) = 
    [
    let mx, my = mouseTile runState |> Option.defaultValue (-1, -1)

    let productTiles = 
        corps 
        |> List.collect (fun c -> 
            productTiles None c.headOffice
            |> List.map (fun (x, y, q) -> (x, y), (c, q)))
        |> List.groupBy fst
        |> List.map (fun (pos, tiles) -> 
            let best = tiles |> List.sortByDescending (fun (_, (_, q)) -> q) |> List.head |> snd
            pos, best)
        |> Map.ofList

    yield! 
        Set.toList map 
        |> List.collect (fun (x, y) -> [
            let rect = isoRect x y tw th
            match Map.tryFind (x, y) productTiles with
            | Some (c, _) -> yield 0, Image ("tile", rect, c.colour)
            | None -> yield 0, Image ("tile", rect, Color.White)
            if (x, y) = (mx, my) then
                yield 2, Image ("tile-highlight", rect, Color.White)
            ])

    yield! 
        corps 
        |> List.collect (fun corp -> [
            let o = corp.headOffice
            let rect = isoRect o.x o.y tw (th*3)
            yield 1, Image ("office", rect, corp.colour)
            if (o.x, o.y) = (mx, my) then
                yield 3, Image ("office-highlight", rect, Color.White)
            ])
  
    ] 
    |> List.sortBy fst 
    |> List.map snd 
    |> fun l -> List.append l (getUIView runState ui)