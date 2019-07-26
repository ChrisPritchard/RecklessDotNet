namespace Xelmish.UI

module Model =

    type Element = {
        elementType: ElementType
        attributes: Attribute list
    }
    and ElementType =
        | Row of children: Element list
        | Column of children: Element list
        | Span
        | Button
    and Attribute = 
        | Style of style: (Style -> Style)
        | Width of int
        | Height of int
        | Text of string
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
    }
    and Colour = byte * byte * byte * byte

    let col attributes children = { elementType = Column children; attributes = attributes }
    let row attributes children = { elementType = Row children; attributes = attributes }
    let span attributes = { elementType = Span; attributes = attributes } 
    let button attributes = { elementType = Button; attributes = attributes }

    let text s = Text s
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

    let testModel = 
        col [] [
            span [ fontSize 20.; text "Cell 1" ]
            span [ text "This is some sample text" ]
            button [ text "Click Me"; onclick (fun _ -> ()) ]
        ]

    let rec render style topLeft spaceToFill element = 
        let x, y = topLeft
        let width, height = spaceToFill
        let newStyle = (style, element.attributes) ||> List.fold (fun style -> function | Style f -> f style | _ -> style)

        let rec renderRow left spaceRemaining elementsRemaining =
            match elementsRemaining with
            | [] -> ()
            | element::rest ->
                let width = 
                    element.attributes 
                    |> List.tryPick (function Width x -> Some x | _ -> None) 
                    |> Option.defaultValue (spaceRemaining / elementsRemaining.Length)
                render newStyle (left, y) (width, height) element
                renderRow (left + width) (spaceRemaining - width) rest

        let rec renderCol top spaceRemaining elementsRemaining =
            match elementsRemaining with
            | [] -> ()
            | element::rest ->
                let height = 
                    element.attributes 
                    |> List.tryPick (function Height x -> Some x | _ -> None) 
                    |> Option.defaultValue (spaceRemaining / elementsRemaining.Length)
                render newStyle (x, top) (width, height) element
                renderRow (top + height) (spaceRemaining - height) rest

        match element.elementType with
        | Row children -> renderRow x width children
        | Column children -> renderCol y height children
        | Span ->
            // draw in place (children are overlayed)
            ()
        | Button ->
            // draw in place (default padding)?
            ()