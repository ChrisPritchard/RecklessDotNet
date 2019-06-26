module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ShowWindow of Window
    | CloseWindow
    | TargetOrder of Order

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = (x, y) }, Cmd.none
    | ShowWindow window -> { model with window = Some window }, Cmd.none
    | CloseWindow -> { model with window = None }, Cmd.none
    | TargetOrder _ -> failwith "TODO"