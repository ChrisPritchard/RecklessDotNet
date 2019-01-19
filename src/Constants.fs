module Constants

open Microsoft.Xna.Framework
open GameCore.GameModel
open GameCore.UIElements.Common
open Model
open GameCore.UIElements.Button

let windowSize = 800, 600
let winw, winh = windowSize
let tileSize = 64, 32
let tw, th = tileSize
let maxMapSize = 10

let font = "font"
let fontSize = 14
let activeColours = { background = Color.Gray; border = Some (2, Color.DarkGray); text = Color.White }
let inactiveColours = { activeColours with text = Color.WhiteSmoke }
let padding = 10

let defaultButton : Button = {
    destRect = 0, 0, 0, 0
    text = [""]
    fontAsset = font
    fontSize = fontSize
    idleColours = activeColours
    hoverColours = Some { activeColours with background = Color.LightGray }
    pressedColours = Some { activeColours with background = Color.Black }
    state = []
}

let assets = [
        Font ("font", "./content/SourceCodePro-Regular")
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