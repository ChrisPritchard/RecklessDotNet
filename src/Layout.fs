module Layout

/// Divides a rectangle into rows and columns, with rows and columns given as float percentages.
/// Ignores rows/columns that do not fix, and/or truncates them as necessary. 
/// Also returns remainder space as a rect if the outer rect is not totally consumed.
/// Returns a list of subrects in the same x,y,w,h tuple format as the outer rect.
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
    
/// Simple mapping function on a rect to return its position.
/// Use with rowsAndCols or similar if you don't care about width and height
let topLeft (x, y, _, _) = x, y

/// Simple function to return the point at the middle of a rect.
/// Useful when placing centre-aligned text.
let middle (x, y, w, h) = x + w / 2, y + h / 2

/// Takes a rect and splits it down the middle into left and right halves, returned as an array.
let splitVertically (x, y, w, h) =
    [|(x, y, w/2, h); (x + w/2, y, w/2, h)|]

/// Takes a rect and splits it across the middle into top and bottom halves, returned as an array. 
let splitHorizontally (x, y, w, h) =
    [|(x, y, w, h/2); (x, y + h/2, w, h/2)|]

/// Returns a rectangle that is within the out rectangle by a margin. 
/// Also returns a rectangle inside this first rectangle by a padding amount.
let marginPad margin padding (x, y, w, h) =
    (x + margin, y + margin, w - (margin * 2), h - (margin * 2)),
    (x + margin + padding, y + margin + padding, w - ((margin + padding) * 2), h - ((margin + padding) * 2))
