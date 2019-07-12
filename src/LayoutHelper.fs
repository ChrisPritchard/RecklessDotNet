module LayoutHelper

/// Divides a rectangle into rows and columns, with rows and columns given as float percentages.
/// Ignores rows/columns that do not fix, and/or truncates them as necessary. 
/// Also returns remainder space as a rect if the outer rect is not totally consumed.
/// Returns a list of subrects in the same x,y,w,h tuple format as the outer rect.
/// Note: returns rects col by col
let rowsAndCols (rows: float list) (cols: float list) (x, y, w, h) =
    let folder (results, remainder) segment = 
        if remainder = 0 then results, remainder
        elif remainder < segment then remainder::results, 0
        else segment::results, remainder - segment
    
    let rowHeights = 
        Seq.map (fun rowDef -> int (rowDef * float h)) rows
        |> Seq.fold folder ([], h) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res
        |> Seq.rev

    let colWidths = 
        Seq.map (fun colDef -> int (colDef * float w)) cols
        |> Seq.fold folder ([], w) 
        |> fun (res, rem) -> if rem <> 0 then rem::res else res 
        |> Seq.rev
        |> Seq.toList // colWidths is iterated over repeatedly, so compute once

    let colFolder currentTop height currentLeft width =
        (currentLeft, currentTop, width, height), currentLeft + width
    let rowFolder currentTop height =
        Seq.mapFold (colFolder currentTop height) x colWidths |> fst, currentTop + height

    Seq.mapFold rowFolder y rowHeights 
    |> fst |> Seq.collect id 
    |> Seq.sortBy (fun (x, y, _, _) -> x, y) 
    |> Seq.toArray
    
let topLeft (x, y, _, _) = x, y
let topRight (x, y, w, _) = x + w, y
let middle (x, y, w, h) = x + w / 2, y + h / 2

/// Returns a rectangle that is within the out rectangle by a margin. 
let contractBy margin (x, y, w, h) =
    x + margin, y + margin, w - (margin * 2), h - (margin * 2)

let contains (tx, ty) (x, y, w, h) =
    tx >= x && tx < x + w &&
    ty >= y && ty < y + h