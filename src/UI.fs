module Reckless.UI

open Microsoft.Xna.Framework
open GameCore.GameModel
open GameCore.UIElements.Model
open GameCore.UIElements.Functions

// let standardColours = { background = Color.DarkBlue; border = Some (3, Color.Blue); text = Color.White }

// let table destRect header rows = 
//     Panel ({
//         destRect = destRect
//         background = Some standardColours.background
//         border = standardColours.border
//         padding = Some 10
//         alignment = Some (AlignVertically 10)
//     }, [ 
//         Label {  }
//         ])

let updateUIModel runState ui =
    [
        yield Label { 
            text = [sprintf "%i, %i" <|| runState.mouse.position]
            fontAsset = "font"
            destRect = (10, 10, 100, 40)
            colours = { background = Color.DarkBlue; border = Some (3, Color.Blue); text = Color.White }
            }
    ]

let getUIView runState ui = 
    let nextUI = List.collect (getElementView runState) ui
    let isPressed = isMousePressed (true, false) runState
    let mx, my = runState.mouse.position 
    nextUI @ [Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))]
    