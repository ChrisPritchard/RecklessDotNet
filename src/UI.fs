module Reckless.UI

open Microsoft.Xna.Framework
open GameCore.GameModel
open GameCore.UIElements.Common
open GameCore.UIElements.Button

let updateUIModel _ ui = ui
    
let mousePos runState = { 
    text = [sprintf "%i, %i" <|| runState.mouse.position]
    fontAsset = "font"
    fontSize = 16
    destRect = (10, 10, 140, 40)
    idleColours = { background = Color.DarkBlue; border = Some (3, Color.Blue); text = Color.White }
    hoverColours = None
    pressedColours = None
    state = []
    }

let getUIView runState ui = [
    yield! getButtonView <| mousePos runState

    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    yield Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))
]