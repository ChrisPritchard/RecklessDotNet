module Main.Update

open Elmish
open Model

let update message model = 
    match message with
    | SelectTile (x, y) -> { model with selectedTile = Some (x, y) }, Cmd.none
    | DeselectTile -> { model with selectedTile = None }, Cmd.none