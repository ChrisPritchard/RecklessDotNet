module StartModel

open Xelmish.Model
open SharedModel

let private dim = Constants.maxMapSize

let startMarket = 
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

let private subOffice2 = {
    x = 1
    y = 3
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
    managedOffices = [subOffice2]
    departments = [Product 16; Marketing; Product 30]
    extensions = []
}

let private testOffice5 = {
    x = 9
    y = 7
    managedOffices = []
    departments = [Research; Research; Research; Marketing; Product 100; Acquisitions]
    extensions = []
}

let private testOffice4 = {
    x = 9
    y = 8
    managedOffices = [testOffice5]
    departments = [Research; Research; Research; Marketing; Product 100; Acquisitions]
    extensions = []
}

let private testOffice3 = {
    x = 8
    y = 8
    managedOffices = [testOffice4]
    departments = [Research; Research; Research; Marketing; Product 100; Acquisitions]
    extensions = []
}

let player = {   
    name = "Aquinas Corp"
    abbreviation = "AQU"
    cash = 5000
    ideas = 0
    headOffice = testOffice2
    orders = []
    colour = Colour.Yellow
}

let others = [
    {   
        name = "Evil Corp"
        abbreviation = "EVL"
        cash = 5000
        ideas = 0
        headOffice = testOffice
        orders = []
        colour = Colour.Purple
    }
    {   
        name = "Jade Systems"
        abbreviation = "JAS"
        cash = 5000
        ideas = 0
        headOffice = testOffice3
        orders = []
        colour = Colour.Green
    }
]