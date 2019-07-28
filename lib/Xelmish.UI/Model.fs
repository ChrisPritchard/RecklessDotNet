namespace Xelmish.UI

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
        | Width of Size
        | Height of Size
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
        alignment: float
        enabled: bool
    }
    and Colour = byte * byte * byte * byte
    and Size = Percent of float | Pixels of int

    let col attributes children = { elementType = Column children; attributes = attributes }
    let row attributes children = { elementType = Row children; attributes = attributes }
    let text attributes s = { elementType = Text s; attributes = attributes } 
    let button attributes s = { elementType = Button s; attributes = attributes }

    let onclick f = OnClick f
    let width i = Width i
    let height i = Height i

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
    let alignment s = Style (fun style -> { style with alignment = s })
    let enabled s = Style (fun style -> { style with enabled = s })

    let testModel = 
        col [] [
            text [ fontSize 20.; margin (px 10) ] "Cell 1"
            text [ ] "This is some sample text"
            row [ padding (pct 0.1) ] [
                button [ onclick (fun _ -> ()) ] "Click Me"
                button [ enabled false ] "Can't click Me"
            ]
        ]

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

    let private renderColour (x, y) (width, height) colour = []

    let private renderText style (x, y) (width, height) text = []

    let private renderButton style (x, y) (width, height) text = []

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

            if style.borderSize > 0 then
                yield! renderColour (x, y) (width, height) style.borderColour

            let x, y = x + style.borderSize, y + style.borderSize
            let width, height = width - (2 * style.borderSize), height - (2 * style.borderSize)
            yield! renderColour (x, y) (width, height) style.backgroundColour

            let topPadding, leftPadding = 
                match style.padding with 
                | Pixels n -> n, n 
                | Percent p -> int (p * float height), int (p * float width)
            let x, y = x + leftPadding, y + topPadding
            let width, height = width - (2 * leftPadding), height - (2 * topPadding)

            match element.elementType with
            | Row children -> 
                let renderImpl = fun left width child -> render newStyle (left, y) (width, height) child
                yield! renderRow renderImpl x width width children
            | Column children -> 
                let renderImpl = fun top height child -> render newStyle (x, top) (width, height) child
                yield! renderCol renderImpl y height height children
            | Text s -> 
                yield! renderText newStyle (x, y) (width, height) s
            | Button s -> 
                yield! renderButton newStyle (x, y) (width, height) s
        ]

    //let renderElements = render 