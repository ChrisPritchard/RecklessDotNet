module Constants

open Xelmish.Model
open Xelmish.Viewables

// Global constants

let windowSize = 800, 600
let windowWidth, windowHeight = windowSize
let tileWidth, tileHeight = 64, 32
let maxMapSize = 10

let centreX = (windowWidth - (tileWidth * maxMapSize)) / 2 + (tileWidth/2)
let centreY = windowHeight / 3 + (tileHeight/2)

let quitOnEscape = true

// Common UI constants and config

let colours = {|
        background = Colour.DarkGray
        text = Colour.White
        button = Colour.Blue
        buttonDisabled = Colour.Gray
        buttonHover = Colour.LightBlue
        buttonPressed = Colour.DarkBlue
    |}
let normalText = text "defaultFont" 16. colours.text
let titleText = text "defaultFont" 25. colours.text
let buttonText = text "defaultFont" 16. colours.text