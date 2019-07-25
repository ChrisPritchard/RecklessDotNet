namespace Xelmish.UI

module Model =

    type Element = {
        elementType: ElementType
        attributes: Attribute list
        children: Element list
    }
    and ElementType =
        | Row
        | Column
        | Span
        | Button
    and Attribute = 
        | Style of style: (Style -> Style)
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

    let private shim elementType attributes children = { elementType = elementType; attributes = attributes; children = children }

    let col = shim Column
    let row = shim Row
    let span = shim Span
    let button = shim Button

    let text s = Text s
    let onclick f = OnClick f
    
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
            span [ fontSize 20.; text "Cell 1" ] []
            span [ text "This is some sample text" ] []
            button [ text "Click Me"; onclick (fun _ -> ()) ] []
        ]