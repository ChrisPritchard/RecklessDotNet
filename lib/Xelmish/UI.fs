module Xelmish.UI

open Model
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Element = {
    elementType: ElementType
    attributes: Attribute list
}
and ElementType =
    | Row of children: Element list
    | Column of children: Element list
    | Image of key: string
    | Text of string
    | Button of text: string
and Attribute = 
    | GlobalStyle of style: (GlobalStyle -> GlobalStyle)
    | LocalStyle of style: (LocalStyle -> LocalStyle)
    | Width of Size
    | Height of Size
    | X of Size
    | Y of Size
    | OnClick of event: (unit -> unit)
and GlobalStyle = {
    fontName: string
    fontSize: float
    colour: Colour
    enabled: bool    
}
and LocalStyle = {
    backgroundColour: Colour
    margin: Size
    padding: Size
    borderSize: int
    borderColour: Colour
    alignment: float * float
}
and Size = Percent of float | Pixels of int

let col attributes children = { elementType = Column children; attributes = attributes }
let row attributes children = { elementType = Row children; attributes = attributes }
let image attributes k = { elementType = Image k; attributes = attributes }
let text attributes s = { elementType = Text s; attributes = attributes } 
let button attributes s = { elementType = Button s; attributes = attributes }

let onclick f = OnClick f
let width i = Width i
let height i = Height i
let x i = X i
let y i = Y i

let px n = Pixels n
let pct n = Percent n
    
let fontName s = GlobalStyle (fun style -> { style with fontName = s })
let fontSize s = GlobalStyle (fun style -> { style with fontSize = s })
let colour s = GlobalStyle (fun style -> { style with colour = s })
let enabled s = GlobalStyle (fun style -> { style with enabled = s })

let defaultLocalStyle = {
    backgroundColour = Colour.Transparent
    margin = px 0
    padding = px 0
    borderSize = 0
    borderColour = Colour.Transparent
    alignment = 0., 0.
}

let backgroundColour s = LocalStyle (fun style -> { style with backgroundColour = s })
let margin s = LocalStyle (fun style -> { style with margin = s })
let padding s = LocalStyle (fun style -> { style with padding = s })
let borderSize s = LocalStyle (fun style -> { style with borderSize = s })
let borderColour s = LocalStyle (fun style -> { style with borderColour = s })
let alignment x y = LocalStyle (fun style -> { style with alignment = (x, y) })

let rec private renderRow renderImpl left totalSpace spaceRemaining childrenRemaining =
    [
        match childrenRemaining with
        | [] -> ()
        | child::rest ->
            let width = 
                child.attributes 
                |> List.tryPick (function 
                    | Width (Pixels x) -> Some x 
                    | Width (Percent x) -> Some (x * float totalSpace |> int) 
                    | _ -> None) 
                |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
            yield! renderImpl left width child
            yield! renderRow renderImpl (left + width) totalSpace (spaceRemaining - width) rest
    ]
    
let rec private renderCol renderImpl top totalSpace spaceRemaining childrenRemaining =
    [ 
        match childrenRemaining with
        | [] -> ()
        | child::rest ->
            let height = 
                child.attributes 
                |> List.tryPick (function 
                    | Height (Pixels x) -> Some x 
                    | Height (Percent x) -> Some (x * float totalSpace |> int) 
                    | _ -> None) 
                |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
            yield! renderImpl top height child
            yield! renderCol renderImpl (top + height) totalSpace (spaceRemaining - height) rest 
    ]

let private renderColour (x, y) (width, height) colour =
    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.whiteTexture, rect x y width height, colour))

let private renderImage globalStyle (x, y) (width, height) key =
    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.textures.[key], rect x y width height, globalStyle.colour))

let private renderText globalStyle localStyle (x, y) _ (text: string) = 
    OnDraw (fun loadedAssets _ spriteBatch -> 
        let font = loadedAssets.fonts.[globalStyle.fontName]
        let measured = font.MeasureString (text)
        let scale = let v = float32 globalStyle.fontSize / measured.Y in Vector2(v, v)
        let ox, oy = localStyle.alignment
        let origin = Vector2 (float32 ox * measured.X * scale.X, float32 oy * measured.Y * scale.Y)
        let position = Vector2.Add(origin, Vector2(float32 x, float32 y))
        spriteBatch.DrawString (font, text, position, globalStyle.colour, 0.f, Vector2.Zero, scale, SpriteEffects.None, 0.f))
        
let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let rec render globalStyle (x, y) (width, height) element = 
    [
        let globalStyle, localStyle = 
            ((globalStyle, defaultLocalStyle), element.attributes) 
            ||> List.fold (fun (globalStyle, localStyle) -> 
                function 
                | GlobalStyle f -> f globalStyle, localStyle 
                | LocalStyle f -> globalStyle, f localStyle
                | _ -> globalStyle, localStyle)

        let topMargin, leftMargin = 
            match localStyle.margin with 
            | Pixels n -> n, n 
            | Percent p -> int (p * float height), int (p * float width)
        let x, y = x + leftMargin, y + topMargin
        let width, height = width - (2 * leftMargin), height - (2 * topMargin)
        
        let onClick = List.tryPick (function OnClick e -> Some e | _ -> None) element.attributes
        match onClick with
        | Some e when globalStyle.enabled ->
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
        yield renderColour (x, y) (width, height) localStyle.backgroundColour

        let topPadding, leftPadding = 
            match localStyle.padding with 
            | Pixels n -> n, n 
            | Percent p -> int (p * float height), int (p * float width)
        let dx, dy = x + leftPadding, y + topPadding
        let width, height = width - (2 * leftPadding), height - (2 * topPadding)

        let x, y = 
            ((dx, dy), element.attributes) 
            ||> List.fold (fun (x, y) -> 
                function
                | X (Pixels ox) -> dx + ox, y
                | X (Percent ox) -> dx + int (ox * float width), y
                | Y (Pixels oy) -> x, dy + oy
                | Y (Percent oy) -> x, dy + int (oy * float height)
                | _ -> x, y) 

        match element.elementType with
        | Row children -> 
            let renderImpl = fun left width child -> render globalStyle (left, y) (width, height) child
            yield! renderRow renderImpl x width width children
        | Column children -> 
            let renderImpl = fun top height child -> render globalStyle (x, top) (width, height) child
            yield! renderCol renderImpl y height height children
        | Text s | Button s -> 
            yield renderText globalStyle localStyle (x, y) (width, height) s
        | Image key ->
            yield renderImage globalStyle (x, y) (width, height) key
    ]