module Constants

open Xelmish.Model
open Xelmish.UI

// Global constants

let windowSize = 800, 600
let windowWidth, windowHeight = windowSize
let tileWidth, tileHeight = 64, 32
let maxMapSize = 10

let centreX = (windowWidth - (tileWidth * maxMapSize)) / 2 + (tileWidth/2)
let centreY = windowHeight / 3 + (tileHeight/2)

let quitOnEscape = true

// Common UI constants and config

let defaultPadding = padding (px 5)
let defaultMargin = margin (px 5)

let colours = {|
    clearColour = Colour.DarkGray
    temp = Colour.Magenta
    background = Colour (50, 50, 50)
    text = Colour.White
    itemChangedText = Colour.Red
    button = Colour.Blue
    buttonDisabled = Colour.Gray
    buttonHover = Colour.LightBlue
    buttonPressed = Colour.DarkBlue
|}