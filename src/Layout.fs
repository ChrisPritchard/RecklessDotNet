module Layout

let divide (left, top) (totalWidth, totalHeight) (rows: float list) (cols: float list) =
    [
        let mutable rowTop = top
        let mutable heightRemainder = totalHeight

        for rowDefinition in rows do
            if heightRemainder <= 0 then ()
            else
                let height = 
                    let target = int (float totalHeight * rowDefinition)
                    if target > heightRemainder then heightRemainder else target
                heightRemainder <- heightRemainder - height

                let mutable colLeft = left
                let mutable widthRemainder = totalWidth

                for colDefinition in cols do
                    if widthRemainder <= 0 then ()
                    else
                        let width = 
                            let target = int (float totalWidth * colDefinition)
                            if target > widthRemainder then widthRemainder else target
                        widthRemainder <- widthRemainder - width

                        yield (colLeft, rowTop, width, height)
                        colLeft <- colLeft + width

                rowTop <- rowTop + height
    ]