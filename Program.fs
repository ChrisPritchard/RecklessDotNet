open Model
open GameCore.GameModel
open GameCore.GameRunner
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

[<EntryPoint>]
let main _ =
    
    let market = {
        width = 20
        height = 20
        buildings = [
            { x = 3; y = 3; owner = "red"; departments = [ Product 100; Product 100 ] }
            { x = 5; y = 7; owner = "blue"; departments = [ Product 100 ] }
            { x = 12; y = 12; owner = "red"; departments = [ Product 100; Product 100; Product 100 ] }
            { x = 15; y = 8; owner = "blue"; departments = [ Product 100; Product 100; Marketing ] }
            { x = 18; y = 6; owner = "red"; departments = [ Product 100 ] }
        ]
    }

    let config = {
        clearColour = Some Color.Black
        fpsFont = None
        assetsToLoad = []
        resolution = Windowed (800, 800)
    }

    let tileSize = 40
    let rectFor x y = x * tileSize, y * tileSize, tileSize, tileSize
    let colourFor = function "red" -> Color.Red | _ -> Color.Blue

    let advanceModel runState _ = 
        if wasJustPressed Keys.Escape runState then None else Some market

    runGame config advanceModel (fun _ m ->
        let buildingLocs = m.buildings |> List.map (fun b -> b.x, b.y)
        [1..m.width] |> List.collect (fun x -> 
            [1..m.height] |> List.map (fun y ->
                let rect = rectFor x y
                if List.contains (x, y) buildingLocs then 
                    Colour (rect, Color.Gray)
                else
                    match owner x y m with
                    | None -> Colour (rect, Color.Black)
                    | Some o -> Colour (rect, colourFor o))))
    0
