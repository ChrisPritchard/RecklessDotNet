open GameCore.GameModel
open GameCore.ImGui.GameRunner
open Microsoft.Xna.Framework

[<EntryPoint>]
let main _ =
    
    let config = {
        resolution = Windowed Constants.windowSize
        clearColour = Some Color.Black
        fpsFont = None
        assetsToLoad = Constants.assets
        mouseVisible = true
    }

    runImGuiGame config Update.advanceModel View.getView (fun _ -> (), [])

    0