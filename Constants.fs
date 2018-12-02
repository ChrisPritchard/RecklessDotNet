module Constants

open Model

let cost =
    function
    | Product _ -> 0
    | Marketing -> 2500
    | Research -> 2500
    | Acquisitions -> 3000