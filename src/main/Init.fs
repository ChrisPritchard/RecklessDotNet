﻿module Main.Init

open Xelmish.Model
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
}

let private subOffice2 = {
    x = 1
    y = 3
    managedOffices = []
    departments = [Product 16; Product 30]
}

let private testOffice = {
    x = 4
    y = 4
    managedOffices = [subOffice]
    departments = [Product 16; Marketing; Product 22; Product 30]
}

let private aquinasHeadOffice = {
    x = 2
    y = 7
    managedOffices = [subOffice2]
    departments = [Product 16; Marketing; Product 30; Admin (Some { name = "Christopher Aquinas" })]
}

let private testOffice5 = {
    x = 9
    y = 7
    managedOffices = []
    departments = [Research;Research;Research;Marketing;Product 100]
}

let private testOffice4 = {
    x = 9
    y = 8
    managedOffices = [testOffice5]
    departments = [Research;Research;Research;Marketing;Product 100]
}

let private testOffice3 = {
    x = 8
    y = 8
    managedOffices = [testOffice4]
    departments = [Research;Research;Research;Marketing;Product 100]
}

let player = {   
    name = "Aquinas Corp"
    abbreviation = "AQU"
    cash = 5000
    ideas = 0
    headOffice = aquinasHeadOffice
    colour = Colour.Yellow
}

let private others = [
    {   
        name = "Evil Corp"
        abbreviation = "EVL"
        cash = 5000
        ideas = 0
        headOffice = testOffice
        colour = Colour.Purple
    }
    {   
        name = "Jade Systems"
        abbreviation = "JAS"
        cash = 5000
        ideas = 0
        headOffice = testOffice3
        colour =Colour. Green
    }
]

let init () = 
    let market = {
        tiles = startMarket
        player = player
        others = others }
    { market = market; selectedTile = None; window = None }