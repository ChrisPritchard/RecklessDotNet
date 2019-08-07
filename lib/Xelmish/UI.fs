﻿module Xelmish.UI

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
    | Style of style: (Style -> Style)
    | Width of Size
    | Height of Size
    | X of Size
    | Y of Size
    | OnClick of event: (unit -> unit)
and Style = {
    fontName: string
    fontSize: float
    colour: Colour
    backgroundColour: Colour
    margin: Size
    padding: Size
    borderSize: int
    borderColour: Colour
    alignment: float * float
    enabled: bool
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
    
let fontName s = Style (fun style -> { style with fontName = s })
let fontSize s = Style (fun style -> { style with fontSize = s })
let colour s = Style (fun style -> { style with colour = s })
let backgroundColour s = Style (fun style -> { style with backgroundColour = s })
let margin s = Style (fun style -> { style with margin = s })
let padding s = Style (fun style -> { style with padding = s })
let borderSize s = Style (fun style -> { style with borderSize = s })
let borderColour s = Style (fun style -> { style with borderColour = s })
let alignment x y = Style (fun style -> { style with alignment = (x, y) })
let enabled s = Style (fun style -> { style with enabled = s })

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

let private renderImage style (x, y) (width, height) key =
    OnDraw (fun loadedAssets _ spriteBatch -> 
        spriteBatch.Draw(loadedAssets.textures.[key], rect x y width height, style.colour))

let private renderText style (x, y) _ (text: string) = 
    OnDraw (fun loadedAssets _ spriteBatch -> 
        let font = loadedAssets.fonts.[style.fontName]
        let measured = font.MeasureString (text)
        let scale = let v = float32 style.fontSize / measured.Y in Vector2(v, v)
        let ox, oy = style.alignment
        let origin = Vector2 (float32 ox * measured.X * scale.X, float32 oy * measured.Y * scale.Y)
        let position = Vector2.Add(origin, Vector2(float32 x, float32 y))
        spriteBatch.DrawString (font, text, position, style.colour, 0.f, Vector2.Zero, scale, SpriteEffects.None, 0.f))
        
let private isInside tx ty tw th x y = x >= tx && x <= tx + tw && y >= ty && y <= ty + th

let rec render style (x, y) (width, height) element = 
    [
        let newStyle = 
            ({ style with padding = px 0; margin = px 0 }, element.attributes) 
            ||> List.fold (fun style -> function | Style f -> f style | _ -> style)

        let topMargin, leftMargin = 
            match style.margin with 
            | Pixels n -> n, n 
            | Percent p -> int (p * float height), int (p * float width)
        let x, y = x + leftMargin, y + topMargin
        let width, height = width - (2 * leftMargin), height - (2 * topMargin)
        
        let onClick = List.tryPick (function OnClick e -> Some e | _ -> None) element.attributes
        match onClick with
        | Some e when style.enabled ->
            yield OnUpdate (fun inputs -> 
                if (inputs.mouseState.X, inputs.mouseState.Y) ||> isInside x y width height then
                    if inputs.mouseState.LeftButton = ButtonState.Pressed 
                    && inputs.lastMouseState.LeftButton <> ButtonState.Pressed then
                        e ())
        | _ -> ()

        if style.borderSize > 0 then
            yield renderColour (x, y) (width, height) style.borderColour

        let x, y = x + style.borderSize, y + style.borderSize
        let width, height = width - (2 * style.borderSize), height - (2 * style.borderSize)
        yield renderColour (x, y) (width, height) style.backgroundColour

        let topPadding, leftPadding = 
            match style.padding with 
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
            let renderImpl = fun left width child -> render newStyle (left, y) (width, height) child
            yield! renderRow renderImpl x width width children
        | Column children -> 
            let renderImpl = fun top height child -> render newStyle (x, top) (width, height) child
            yield! renderCol renderImpl y height height children
        | Text s | Button s -> 
            yield renderText newStyle (x, y) (width, height) s
        | Image key ->
            yield renderImage newStyle (x, y) (width, height) key
    ]