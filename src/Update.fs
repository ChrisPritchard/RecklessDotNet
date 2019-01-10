module Reckless.Update

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open GameCore.GameModel
open Model

let dim = Constants.mapSize
let map = 
    [0..dim*dim-1] 
    |> List.fold (fun map i -> 
        let x = i % dim
        let y = i / dim
        Set.add (x, y) map) Set.empty

let testOffice = {
    x = 0
    y = 0
    managedOffices = []
    departments = [Product 16; Marketing; Product 22; Product 30]
    extensions = []
}

let corps = [
    {   cash = 0
        ideas = 0
        headOffice = testOffice
        orders = []
        colour = Color.Red
    }
]

let startModel = map, corps

let advanceModel runState model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some startModel
        | _ -> model