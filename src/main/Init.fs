module Main.Init

open Xelmish.Model
open Model

let init () = 
    let market = {
        tiles = 
            [0..(10*10)-1] 
            |> List.fold (fun map i -> 
                let x = i % 10
                let y = i / 10
                Set.add (x, y) map) Set.empty
        player = {   
            name = "Aquinas Corp"
            abbreviation = "AQU"
            cash = 5000
            ideas = 0
            headOffice = {
                x = 2
                y = 7
                managedOffices = []
                departments = [Product 16; Marketing; Product 30; Admin (Some { name = "Christopher Aquinas" })]
            }
            colour = Colour.Yellow
        }
        others = [] }
    { market = market; selectedTile = None }