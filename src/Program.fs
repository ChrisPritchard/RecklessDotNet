﻿
open Elmish
open Xelmish.Model
open Constants

type Model = Playing of Main.Model.MainModel

let init () = Playing (Main.Init.init ()), Cmd.none

type Message = PlayingMessage of Main.Update.Message

let update message model =
    match message, model with
    | PlayingMessage message, Playing model ->
        let model, command = Main.Update.update message model
        Playing model, Cmd.map PlayingMessage command

let view model dispatch =
    match model with
    | Playing model ->
        Main.View.view model (PlayingMessage >> dispatch)
        
[<EntryPoint>]
let main _ =

    let assets = [
        FileTexture ("tile", "content/tile.png")
        FileTexture ("tile-highlight", "content/tile-highlight.png")
        FileTexture ("office", "content/office.png")
        FileTexture ("office-highlight", "content/office-highlight.png")
        FileTexture ("office-links", "content/office-links.png")
        FileTexture ("office-link-centre", "content/office-link-centre.png")
        FileTexture ("icon-head-office", "content/icon-head-office.png")
        PipelineFont ("defaultFont", "content/SourceCodePro")
    ]

    Program.mkProgram init update view
    |> Program.withConsoleTrace
    |> Xelmish.Program.runSimpleGameLoop assets windowSize colours.clearColour

    0