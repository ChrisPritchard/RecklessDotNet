
open Elmish
open Xelmish.Model
open Constants

type Model = Playing of Main.Model

let init () = Playing (Main.init ()), Cmd.none

type Message = PlayingMessage of Main.Message

let update message model =
    match message, model with
    | PlayingMessage message, Playing model ->
        let model, command = Main.update message model
        Playing model, Cmd.map PlayingMessage command

let view model dispatch =
    match model with
    | Playing model ->
        Main.view model (PlayingMessage >> dispatch)

[<EntryPoint>]
let main _ =
    
    let assets = [
        FileTexture ("tile", "content/tile.png")
        FileTexture ("tile-highlight", "content/tile-highlight.png")
        FileTexture ("office", "content/office.png")
        FileTexture ("office-highlight", "content/office-highlight.png")
    ]

    Program.mkProgram init update view
    |> Xelmish.Program.runSimpleGameLoop assets (winw, winh) Colour.WhiteSmoke

    0