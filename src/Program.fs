
open Model
open Turn

open Microsoft.Xna.Framework.Input
open GameCore.GameRunner
open GameCore.GameModel

[<EntryPoint>]
let main _ =

    let test = {
        x = 10
        y = 10
        managedOffices = []
        departments = [Product 16; Marketing; Product 22; Product 30]
        extensions = []
    }

    updateQuality test [] |> fun o -> printfn "%A" o.departments
    
    let startModel = ()

    let advanceModel runState model =
        if wasJustPressed Keys.Escape runState then None
        else
            match model with
            | None -> Some startModel
            | _ -> model
    
    let getView _ _ = []

    runWindowedGame (800, 600) [] advanceModel getView

    0
