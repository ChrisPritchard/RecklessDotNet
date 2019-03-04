module Constants

open GameCore.GameModel
open Model

let windowSize = 800, 600
let winw, winh = windowSize
let tileSize = 64, 32
let tw, th = tileSize
let maxMapSize = 10

let turnTransitionTime = 500.

let assets = [
        Texture ("tile", "content/tile.png")
        Texture ("tile-highlight", "content/tile-highlight.png")
        Texture ("office", "content/office.png")
        Texture ("office-highlight", "content/office-highlight.png")
    ]

let productIncome = 100

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