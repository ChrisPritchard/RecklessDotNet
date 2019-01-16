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

let subOffice = {
    x = 8
    y = 4
    managedOffices = []
    departments = [Product 16; Product 30]
    extensions = []
}

let testOffice = {
    x = 4
    y = 4
    managedOffices = [subOffice]
    departments = [Product 16; Marketing; Product 22; Product 30]
    extensions = []
}

let testOffice2 = {
    x = 2
    y = 7
    managedOffices = []
    departments = [Product 16; Marketing; Product 30]
    extensions = []
}

let corps = [
    {   cash = 0
        ideas = 0
        headOffice = testOffice
        orders = []
        colour = Color.Red
    }
    {   cash = 0
        ideas = 0
        headOffice = testOffice2
        orders = []
        colour = Color.Blue
    }
]

let ui = []

let startModel = map, corps, ui

let advanceModel runState model =
    if wasJustPressed Keys.Escape runState then None
    else
        match model with
        | None -> Some startModel
        | Some (map, corps, ui) -> Some (map, corps, ui)