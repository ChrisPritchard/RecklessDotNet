﻿namespace Xelmish.UI

module Model =

    type Element = {
        elementType: ElementType
        attributes: Attribute list
    }
    and ElementType =
        | Row of children: Element list
        | Column of children: Element list
        | Text of string
        | Button of string
    and Attribute = 
        | Style of style: (Style -> Style)
        | Width of int
        | Height of int
        | OnClick of event: (unit -> unit)
    and Style = {
        fontName: string
        fontSize: float
        colour: Colour
        backgroundColour: Colour
        margin: float
        padding: float
        borderSize: float
        borderColour: Colour
        alignment: float
        enabled: bool
    }
    and Colour = byte * byte * byte * byte

    let col attributes children = { elementType = Column children; attributes = attributes }
    let row attributes children = { elementType = Row children; attributes = attributes }
    let text attributes s = { elementType = Text s; attributes = attributes } 
    let button attributes s = { elementType = Button s; attributes = attributes }

    let onclick f = OnClick f
    let width i = Width i
    let height i = Height i
    
    let fontName s = Style (fun style -> { style with fontName = s })
    let fontSize s = Style (fun style -> { style with fontSize = s })
    let colour s = Style (fun style -> { style with colour = s })
    let backgroundColour s = Style (fun style -> { style with backgroundColour = s })
    let margin s = Style (fun style -> { style with margin = s })
    let padding s = Style (fun style -> { style with padding = s })
    let borderSize s = Style (fun style -> { style with borderSize = s })
    let borderColour s = Style (fun style -> { style with borderColour = s })
    let alignment s = Style (fun style -> { style with alignment = s })
    let enabled s = Style (fun style -> { style with enabled = s })

    let testModel = 
        col [] [
            text [ fontSize 20. ] "Cell 1"
            text [ ] "This is some sample text"
            button [ onclick (fun _ -> ()) ] "Click Me"
            button [ enabled false ] "Can't click Me"
        ]

    let rec render style topLeft spaceToFill element = 
        let x, y = topLeft
        let width, height = spaceToFill
        let newStyle = (style, element.attributes) ||> List.fold (fun style -> function | Style f -> f style | _ -> style)

        let rec renderRow left spaceRemaining childrenRemaining =
            match childrenRemaining with
            | [] -> ()
            | child::rest ->
                let width = 
                    child.attributes 
                    |> List.tryPick (function Width x -> Some x | _ -> None) 
                    |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
                render newStyle (left, y) (width, height) child
                renderRow (left + width) (spaceRemaining - width) rest

        let rec renderCol top spaceRemaining childrenRemaining =
            match childrenRemaining with
            | [] -> ()
            | child::rest ->
                let height = 
                    child.attributes 
                    |> List.tryPick (function Height x -> Some x | _ -> None) 
                    |> Option.defaultValue (spaceRemaining / childrenRemaining.Length)
                render newStyle (x, top) (width, height) child
                renderRow (top + height) (spaceRemaining - height) rest

        let renderSpan s = ()
        let renderButton s = ()

        match element.elementType with
        | Row children -> renderRow x width children
        | Column children -> renderCol y height children
        | Text s -> renderSpan s
        | Button s -> renderButton s