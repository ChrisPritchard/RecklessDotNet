module Reckless.StartModel

open Microsoft.Xna.Framework
open Model

let private dim = Constants.maxMapSize

let private startMarket = 
    [0..dim*dim-1] 
    |> List.fold (fun map i -> 
        let x = i % dim
        let y = i / dim
        Set.add (x, y) map) Set.empty

let private subOffice = {
    x = 8
    y = 4
    managedOffices = []
    departments = [Product 16; Product 30]
    extensions = []
}

let private testOffice = {
    x = 4
    y = 4
    managedOffices = [subOffice]
    departments = [Product 16; Marketing; Product 22; Product 30]
    extensions = []
}

let private testOffice2 = {
    x = 2
    y = 7
    managedOffices = []
    departments = [Product 16; Marketing; Product 30]
    extensions = []
}

let private corps = [
    {   
        name = "Evil Corp"
        abbreviation = "EVL"
        cash = 0
        ideas = 0
        headOffice = testOffice
        orders = []
        colour = Color.Purple
    }
    {   
        name = "Aquinas Corp"
        abbreviation = "AQU"
        cash = 0
        ideas = 0
        headOffice = testOffice2
        orders = []
        colour = Color.Yellow
    }
]

let startModel () = {
    market = startMarket
    corps = corps
    productTiles = Map.empty
    selectedTile = None
}