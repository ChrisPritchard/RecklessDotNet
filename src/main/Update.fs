module Main.Update

open Elmish
open Model

type Message = 
    | SelectTile of int * int
    | DeselectTile
    | ShowWindow of Window
    | CloseWindow

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = Some (x, y) }, Cmd.none
    | DeselectTile -> { model with selectedTile = None }, Cmd.none
    | ShowWindow window -> { model with window = Some window }, Cmd.none
    | CloseWindow -> { model with window = None }, Cmd.none