module Layout

/// Divides a rectangle into rows and columns, with rows and columns given as float percentages.
/// Ignores rows/columns that do not fix, and/or truncates them as necessary. 
/// Also returns remainder space as a rect if the outer rect is not totally consumed.
/// Returns a list of subrects in the same x,y,w,h tuple format as the outer rect.
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

/// Returns a rectangle that is within the out rectangle by a margin. 
/// Also returns a rectangle inside this first rectangle by a padding amount.
let marginPad (left, top, width, height) margin padding =
    (left + margin, top + margin, width - (margin * 2), height - (margin * 2)),
    (left + margin + padding, top + margin + padding, width - ((margin + padding) * 2), height - ((margin + padding) * 2))
