module Main.UI

open Common
open Layout

let controlPanelRect = 
    let percentage = 0.3
    rowsAndCols (0, 0, winw, winh) [1. - percentage; percentage] []
    |> Array.last

let corpInfoRect, execRect, selectedInfoRect = 
    rowsAndCols controlPanelRect [] [0.3; 0.3]
    |> fun a -> a.[0], a.[1], a.[2]

let orderButtonOptions = 
    rowsAndCols controlPanelRect [0.2; 0.2; 0.2; 0.2; 0.2] [0.25; 0.25; 0.25; 0.25]