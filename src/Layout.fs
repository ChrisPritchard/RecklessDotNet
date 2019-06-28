module Layout

let divide (left, top, totalWidth, totalHeight) (rows: float list) (cols: float list) =
    let folder (results, remainder) segment = 
        if remainder = 0 then results, remainder
        elif remainder < segment then remainder::results, 0
        else segment::results, remainder - segment
    
    let rowHeights = 
        Seq.map (fun rowDef -> int (rowDef * float totalHeight)) rows
        |> Seq.fold folder ([], totalHeight) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res

    let colWidths = 
        Seq.map (fun colDef -> int (colDef * float totalWidth)) cols
        |> Seq.fold folder ([], totalWidth) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res 
        |> Seq.toList // colWidths is iterated over repeatedly, so compute once

    let colFolder currentTop height currentLeft width =
        (currentTop, currentLeft, width, height), currentLeft + width
    let rowFolder currentTop height =
        Seq.mapFold (colFolder currentTop height) left colWidths |> fst, currentTop + height

    Seq.mapFold rowFolder top rowHeights |> fst |> Seq.collect id |> Seq.toList