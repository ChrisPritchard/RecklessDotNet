/// This module contains a mini-framework for creating User Interface elements.
/// It is modelled to be similar to the Giraffe View Engine, from the F# Giraffe Framework (itself inspired by Suave)
module Xelmish.UI

open Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

/// Note: use the col/row/image/text/button functions rather than these types directly
type Element = {
    elementType: ElementType
    attributes: Attribute list
}
/// Note: use the col/row/image/text/button functions rather than these types directly
and ElementType =
    | Row of children: Element list
    | Column of children: Element list
    | Image of key: string
    | Text of string
    | Button of text: string
/// Note: use the onclick/fontname/colour etc functions than these types directly
and Attribute = 
    | GlobalStyle of style: (GlobalStyle -> GlobalStyle)
    | LocalStyle of style: (LocalStyle -> LocalStyle)
    | OnClick of event: (unit -> unit)
/// Note: use the onclick/fontname/colour etc functions than these types directly
and GlobalStyle = {
    fontName: string
    fontSize: float
    alignment: float * float
    colour: Colour
    buttonColour: Colour
    backgroundColour: Colour
    buttonBackgroundColour: Colour
    enabled: bool    
}
/// Note: use the onclick/fontname/colour etc functions than these types directly
and LocalStyle = {
    margin: Size
    padding: Size
    borderSize: int
    borderColour: Colour
    top: Size
    left: Size
    width: Size option
    height: Size option
}
/// Note: use the px/pct functions than these types directly
and Size = Percent of float | Pixels of int

/// Specifies a column, with its children distributed vertically
let col attributes children = { elementType = Column children; attributes = attributes }
/// Specifies a row, with its children distributed horizontally
let row attributes children = { elementType = Row children; attributes = attributes }
/// Specifies a texture image to be drawn
let image attributes textureKey = { elementType = Image textureKey; attributes = attributes }
/// Specifies some text to render
let text attributes s = { elementType = Text s; attributes = attributes } 
/// Specifies a button (background colour with text) to render
let button attributes s = { elementType = Button s; attributes = attributes }

/// A function to call when the containing element is clicked
let onclick f = OnClick f

/// A size in pixels
let px n = Pixels n
/// A size in percent (of its wrapping container)
let pct n = Percent n
    
let fontName s = GlobalStyle (fun style -> { style with fontName = s })
let fontSize s = GlobalStyle (fun style -> { style with fontSize = s })
let colour s = GlobalStyle (fun style -> { style with colour = s })
let buttonColour s = GlobalStyle (fun style -> { style with buttonColour = s })
let backgroundColour s = GlobalStyle (fun style -> { style with backgroundColour = s })
let buttonBackgroundColour s = GlobalStyle (fun style -> { style with buttonBackgroundColour = s })
let enabled s = GlobalStyle (fun style -> { style with enabled = s })
/// For printed text, what its alignment should be. From 0. to 1. top left to bottom right
let alignment x y = GlobalStyle (fun style -> 
    let x = if abs x > 1. then abs x % 1. else abs x
    let y = if abs y > 1. then abs y % 1. else abs y
    { style with alignment = x, y }) // alignment can only be from 0. to 1.

let margin s = LocalStyle (fun style -> { style with margin = s })
let padding s = LocalStyle (fun style -> { style with padding = s })
let borderSize s = LocalStyle (fun style -> { style with borderSize = s })
/// Border colour if the element's border size is greater than 0
let borderColour s = LocalStyle (fun style -> { style with borderColour = s })
let width i = LocalStyle (fun style -> { style with width = Some i })
let height i = LocalStyle (fun style -> { style with height = Some i })
/// The offset from the top of its wrapping container an element should be drawn
let top i = LocalStyle (fun style -> { style with top = i })
/// The offset from the left of its wrapping container an element should be drawn
let left i = LocalStyle (fun style -> { style with left = i })

let private defaultLocalStyle = {
    margin = px 0
    padding = px 0
    borderSize = 0
    borderColour = Colour.Transparent
    top = px 0
    left = px 0
    width = None
    height = None
}

let private styles globalStyle attributes =
    ((globalStyle, defaultLocalStyle), attributes) 
    ||> List.fold (fun (globalStyle, localStyle) -> 
        function 
        | GlobalStyle f -> f globalStyle, localStyle 
        | LocalStyle f -> globalStyle, f localStyle
        | _ -> globalStyle, localStyle)

let rec private renderRow globalStyle renderImpl left totalSpace spaceRemaining childrenRemaining =
    [
        match childrenRemaining with
        | [] -> ()
        | child::rest ->
            let width = 
                snd (styles globalStyle child.attributes)
                |> fun localStyle -> localStyle.width
                |> Option.bind (function 
                    | Pixels x -> Some x 
                    | Percent x -> Some (x * float totalSpace |> int)) 
                |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
            yield! renderImpl left width child
            yield! renderRow globalStyle renderImpl (left + width) totalSpace (spaceRemaining - width) rest
    ]
    
let rec private renderCol globalStyle renderImpl top totalSpace spaceRemaining childrenRemaining =
    [ 
        match childrenRemaining with
        | [] -> ()
        | child::rest ->
            let height = 
                snd (styles globalStyle child.attributes)
                |> fun localStyle -> localStyle.height
                |> Option.bind (function 
                    | Pixels x -> Some x 
                    | Percent x -> Some (x * float totalSpace |> int)) 
                |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
            yield! renderImpl top height child
            yield! renderCol globalStyle renderImpl (top + height) totalSpace (spaceRemaining - height) rest 
    ]

let private renderColour (x, y) (width, height) colour =
    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.whiteTexture, rect x y width height, colour))

let private renderImage globalStyle (x, y) (width, height) key =
    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.textures.[key], rect x y width height, globalStyle.colour))

let private renderText globalStyle colour (x, y) (width, height) (text: string) = 
    OnDraw (fun loadedAssets _ spriteBatch -> 
        let font = loadedAssets.fonts.[globalStyle.fontName]
        let measured = font.MeasureString (text)
        let scale = let v = float32 globalStyle.fontSize / measured.Y in Vector2(v, v)

        let ox, oy = globalStyle.alignment
        let relWidth, relHeight = float32 (float width * ox), float32 (float height * oy)
        let offWidth, offHeight = float32 ox * measured.X * scale.X, float32 oy * measured.Y * scale.Y

        let origin = Vector2 (relWidth - offWidth, relHeight - offHeight)
        let position = Vector2.Add (origin, Vector2(float32 x, float32 y))

        spriteBatch.DrawString (font, text, position, colour, 0.f, Vector2.Zero, scale, SpriteEffects.None, 0.f))
        
let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let rec render globalStyle (x, y) (width, height) element = 
    [
        let newGlobalStyle, localStyle = styles globalStyle element.attributes            

        let topMargin, leftMargin = 
            match localStyle.margin with 
            | Pixels n -> n, n 
            | Percent p -> int (p * float height), int (p * float width)
        let x, y = x + leftMargin, y + topMargin
        let width, height = width - (2 * leftMargin), height - (2 * topMargin)
        
        let onClick = List.tryPick (function OnClick e -> Some e | _ -> None) element.attributes
        match onClick with
        | Some e when newGlobalStyle.enabled ->
            yield OnUpdate (fun inputs -> 
                if (inputs.mouseState.X, inputs.mouseState.Y) ||> isInside x y width height then
                    if inputs.mouseState.LeftButton = ButtonState.Pressed 
                    && inputs.lastMouseState.LeftButton <> ButtonState.Pressed then
                        e ())
        | _ -> ()

        if localStyle.borderSize > 0 then
            yield renderColour (x, y) (width, height) localStyle.borderColour

        let x, y = x + localStyle.borderSize, y + localStyle.borderSize
        let width, height = width - (2 * localStyle.borderSize), height - (2 * localStyle.borderSize)
        if newGlobalStyle.backgroundColour <> globalStyle.backgroundColour then
            yield renderColour (x, y) (width, height) newGlobalStyle.backgroundColour

        let topPadding, leftPadding = 
            match localStyle.padding with 
            | Pixels n -> n, n 
            | Percent p -> int (p * float height), int (p * float width)
        let dx, dy = x + leftPadding, y + topPadding
        let width, height = width - (2 * leftPadding), height - (2 * topPadding)

        let x = match localStyle.left with Pixels ox -> dx + ox | Percent ox -> dx + int (ox * float width)
        let y = match localStyle.top with Pixels oy -> dy + oy | Percent oy -> dy + int (oy * float height)

        match element.elementType with
        | Row children -> 
            let renderImpl = fun left width child -> render newGlobalStyle (left, y) (width, height) child
            yield! renderRow newGlobalStyle renderImpl x width width children
        | Column children -> 
            let renderImpl = fun top height child -> render newGlobalStyle (x, top) (width, height) child
            yield! renderCol newGlobalStyle renderImpl y height height children
        | Text s ->
            yield renderText newGlobalStyle newGlobalStyle.colour (x, y) (width, height) s
        | Button s -> 
            yield renderColour (x, y) (width, height) newGlobalStyle.buttonBackgroundColour
            yield renderText { newGlobalStyle with alignment = 0.5, 0.5 } newGlobalStyle.buttonColour (x, y) (width, height) s
        | Image key ->
            yield renderImage newGlobalStyle (x, y) (width, height) key
    ]