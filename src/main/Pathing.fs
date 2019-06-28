﻿module Main.Pathing

open Xelmish.Model
open Xelmish.Viewables
open Common
open Iso
open Model

let findPathBetween (sourceOffice: Office) (destOffice: Office) (market: Market) =
    let rec bfs paths visited =
        let next = 
            paths
            |> List.collect (fun path ->
                let (x, y) = List.head path
                [-1, 0; 1, 0; 0, -1; 0, 1]
                |> List.map (fun (dx, dy) -> x + dx, y + dy)
                |> List.filter (fun p -> not (Set.contains p visited))
                |> List.map (fun p -> p::path))
        if next = [] then None 
        else
            match List.tryFind (fun path -> List.head path = destOffice.pos) next with
            | Some path -> Some path
            | _ ->
                let newHeads = paths |> List.map List.head
                let newVisited = List.foldBack Set.add newHeads visited
                bfs next newVisited
    let startVisited = 
        market.allOffices 
        |> List.filter (fun info -> info.office <> destOffice) 
        |> List.map (fun info -> info.office.pos)
        |> Set.ofList
    bfs [[sourceOffice.pos]] startVisited

let rec graphicsForPath path soFar =
    let sprite (px, py) (x, y) =
        OnDraw (fun loadedAssets _ (spriteBatch: SpriteBatch) ->
            let texture = loadedAssets.textures.["office-links"]

            let tx, ty, tw, th = isoRect x y tileWidth tileHeight
            let otx, oty = int (px * float tw), int (py * float th)
            let destRect = rect (tx + otx) (ty + oty) (tw/2) (th/2)

            let sx, sy = int (px * float texture.Width), int (py * float texture.Height)
            let sourceRect = System.Nullable(rect sx sy (texture.Width/2) (texture.Height/2))
            
            spriteBatch.Draw (texture, destRect, sourceRect, Colour.White))

    match path with
    | (cx, cy)::(nx, ny)::rest ->
        let centre = 
            let tx, ty, tw, th = isoRect cx cy tileWidth tileHeight
            image "office-link-centre" Colour.White (tw, th) (tx, ty)
        let current, next =
            if cx = nx && cy < ny then
                sprite (0.5, 0.5) (cx, cy), sprite (0., 0.) (nx, ny)
            elif cx = nx && cy > ny then
                sprite (0., 0.) (cx, cy), sprite (0.5, 0.5) (nx, ny)
            elif cx < nx then
                sprite (0.5, 0.) (cx, cy), sprite (0., 0.5) (nx, ny)
            else
                sprite (0., 0.5) (cx, cy), sprite (0.5, 0.) (nx, ny)
        graphicsForPath ((nx, ny)::rest) (centre::current::next::soFar)
    | _ -> soFar