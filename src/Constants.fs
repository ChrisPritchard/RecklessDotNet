module Reckless.Constants

open Microsoft.Xna.Framework
open GameCore.GameModel
open GameCore.UIElements.Common
open Model

let windowSize = 800, 600
let tileSize = 64, 32
let mapSize = 10

let font = "font"
let fontSize = 14
let activeColours = { background = Color.DarkBlue; border = Some (2, Color.Blue); text = Color.White }
let inactiveColours = { activeColours with text = Color.Gray }
let padding = 10

let assets = [
        Font ("font", "./content/Xolonium")
        Texture ("tile", "content/tile.png")
        Texture ("tile-highlight", "content/tile-highlight.png")
        Texture ("office", "content/office.png")
        Texture ("office-highlight", "content/office-highlight.png")
    ]

let rec orderCost =
    function
    | BuildDepartment (_, Product _) -> 0
    | BuildDepartment (_, Marketing) -> 2500
    | BuildDepartment (_, Research) -> 2500
    | BuildDepartment (_, Acquisitions) -> 3000
    | BuildExtension (_, QA) -> 500
    | ResearchIdea _ -> 1000
    | BuildOffice (_, _, _, dep) -> 
        7500 + 
        orderCost (BuildDepartment (Unchecked.defaultof<Office>, dep))

let departmentCost = 
    function
    | Research -> 500
    | Marketing -> 500
    | Acquisitions -> 500
    | Product _ -> 0