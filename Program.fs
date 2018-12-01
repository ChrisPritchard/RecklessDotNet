open Model

open GameCore.GameModel
open GameCore.GameRunner

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

[<EntryPoint>]
let main _ =

    let office1 = {
        x = 0
        y = 0
        departments = [
            Research
            Research
            Marketing
        ]
        extensions = []
    }
    
    // let config = {
    //     clearColour = Some Color.Black
    //     fpsFont = None
    //     assetsToLoad = [
    //         Texture ("tile", "tile.png")
    //         Texture ("office", "office.png")
    //     ]
    //     resolution = Windowed (800, 800)
    // }

    // let advanceModel runState _ = 
    //     if wasJustPressed Keys.Escape runState then None else Some ()
    
    // let getView _ _ = []

    // runGame config advanceModel getView
    0
