
open GameCore.GameRunner

[<EntryPoint>]
let main _ =
  
    runWindowedGame Constants.windowSize Constants.assets Update.advanceModel View.getView

    0