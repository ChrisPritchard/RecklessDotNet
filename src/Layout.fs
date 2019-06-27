module Layout

let divide (left, top) (totalWidth, totalHeight) (rows: float list) (cols: float list) =
    let folder (results, remainder) segment = 
        if remainder = 0 then results, remainder
        elif remainder < segment then remainder::results, 0
        else segment::results, remainder - segment
    
    let rowHeights = 
        Seq.map (fun rowDef -> int (rowDef * float totalHeight)) rows
        |> Seq.fold folder ([], totalHeight) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res

    let colWidths = 
        Seq.map (fun colDef -> int (colDef * float totalWidth)) rows
        |> Seq.fold folder ([], totalWidth) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res 
        |> Seq.toList

    [
        let mutable rowTop = top
        for height in rowHeights do
            let mutable colLeft = left
            for width in colWidths do
                yield (colLeft, rowTop, width, height)
                colLeft <- colLeft + width
            rowTop <- rowTop + height
    ]

let test = divide (0, 0) (100, 100) [0.2; 0.8] [0.2; 0.8]