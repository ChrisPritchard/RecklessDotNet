
open Model
open Turn

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open GameCore.GameRunner
open GameCore.GameModel

[<EntryPoint>]
let main _ =

    let tw, th = 64, 32
    let dim = 10
    let map = 
        [0..dim*dim-1] 
        |> List.fold (fun map i -> 
            let x = i % dim
            let y = i / dim
            Set.add (x, y) map) Set.empty

    let testOffice = {
        x = 0
        y = 0
        managedOffices = []
        departments = [Product 16; Marketing; Product 22; Product 30]
        extensions = []
    }

    let corps = [
        {   cash = 0
            ideas = 0
            headOffice = testOffice
            orders = []
            colour = Color.Red
        }
    ]
    
    let startModel = map, corps

    let advanceModel runState model =
        if wasJustPressed Keys.Escape runState then None
        else
            match model with
            | None -> Some startModel
            | _ -> model
    
    let iso x y cx cy = 
        cx + (x * tw/2 + y*tw/2), cy + (y*th/2 - x*th/2)
    
    let bottomCentre x y w h =
        x - (w/2), y - h

    let getView runState (map, corps) = 
        (Set.toList map |> List.map (fun (x, y) -> 
            let ix, iy = iso x y 96 300
            let ix, iy = bottomCentre ix iy tw th
            let destRect = ix, iy, tw, th
            Image ("tile", destRect, Color.White)))
        @
        (corps |> List.map (fun c -> 
            let ix, iy = iso c.headOffice.x c.headOffice.y 96 300
            let ix, iy = bottomCentre ix iy tw (th*3)
            let destRect = ix, iy, tw, th*3
            Image ("office", destRect, c.colour)))
        @ 
        [
            let isPressed = isMousePressed (true, false) runState
            let mx, my = runState.mouse.position 
            yield Colour ((mx, my, 5, 5), (if isPressed then Color.Red else Color.Yellow))
        ]

    let assets = [
        Texture ("tile", "content/tile.png")
        Texture ("office", "content/office.png")
    ]

    runWindowedGame (800, 600) assets advanceModel getView

    0
