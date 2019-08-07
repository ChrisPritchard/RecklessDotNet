﻿module Main.Update

open Elmish
open Model
open Orders

type Message = 
    | SelectTile of int * int
    | ChangeInterfaceMode

let update message model = 
    match message, model.currentInterface with
    | SelectTile (x, y), Information _ when model.market.tiles.Contains (x, y) -> 
        { model with currentInterface = Information (x, y) }, Cmd.none
    | ChangeInterfaceMode, _ -> { model with newInterfaceMode = not model.newInterfaceMode }, Cmd.none
    | _ -> model, Cmd.none