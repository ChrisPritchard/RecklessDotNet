namespace Xelmish.UI.Model

type Element = {
    elementType: ElementType
    attributes: Attribute list
    children: Element list
}
and ElementType =
    | Row
    | Column
    | Text
    | Button
and Attribute = 
    | Style of style: Style
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