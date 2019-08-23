module Main.Init

open Xelmish.Model
open Model

let private dim = Constants.maxMapSize

let playerExec = { 
    firstName = "Christopher"
    lastName = "Aquinas"
    experience = 3
    level = 6 }
let evilExec = { 
    firstName = "Quinton"
    lastName = "Charlemagne"
    experience = 2
    level = 4 }
let jadeExec = { 
    firstName = "Satya"
    lastName = "Vishal"
    experience = 1
    level = 3 }

let private startMarket = 
    [0..dim*dim-1] 
    |> List.fold (fun map i -> 
        let x = i % dim
        let y = i / dim
        Set.add (x, y) map) Set.empty
               
let private aquinasSubOffice = {
    x = 1
    y = 3
    managedOffices = []
    departments = [Product 16; Product 30; Research]
}

let private aquinasHeadOffice = {
    x = 2
    y = 7
    managedOffices = [aquinasSubOffice]
    departments = [Product 16; Marketing; Product 30; Admin (Some playerExec)]
}

let private evilSubOffice = {
    x = 8
    y = 4
    managedOffices = []
    departments = [Product 16; Product 30]
}

let private evilHeadOffice = {
    x = 4
    y = 4
    managedOffices = [evilSubOffice]
    departments = [Product 16; Marketing; Product 22; Product 30; Admin (Some evilExec)]
}

let private jadeSubSubOffice = {
    x = 9
    y = 7
    managedOffices = []
    departments = [Research;Research;Research;Marketing;Product 100]
}

let private jadeSubOffice = {
    x = 9
    y = 8
    managedOffices = [jadeSubSubOffice]
    departments = [Research;Research;Research;Marketing;Product 100]
}

let private jadeHeadOffice = {
    x = 8
    y = 8
    managedOffices = [jadeSubOffice]
    departments = [Research;Research;Research;Marketing;Product 100; Admin (Some jadeExec)]
}

let player = {   
    displayName = "Aquinas Corp"
    abbreviation = "AQU"
    cash = 5000
    ideas = 1
    headOffice = aquinasHeadOffice
    colour = Colour.Yellow
}

let private others = [
    {   
        displayName = "Evil Corp"
        abbreviation = "EVL"
        cash = 6000
        ideas = 2
        headOffice = evilHeadOffice
        colour = Colour.Purple
    }
    {   
        displayName = "Jade Systems"
        abbreviation = "JAS"
        cash = 4000
        ideas = 3
        headOffice = jadeHeadOffice
        colour =Colour. Green
    }
]

let init () = 
    let market = {
        tiles = startMarket
        player = player
        others = others }
    { 
        market = market
        selectedTile = player.headOffice.pos
        playerAction = Overview }