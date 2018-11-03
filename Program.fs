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
            { x = 12; y = 12; owner = "red"; departments = [ Product 100; Product 100; Product 100; Product 100 ] }
            { x = 15; y = 8; owner = "blue"; departments = [ Product 100; Product 100; Product 100; Marketing ] }
            { x = 18; y = 6; owner = "red"; departments = [ Product 100 ] }
        ]
    }

    let config = {
        clearColour = Some Color.Black
        fpsFont = None
        assetsToLoad = [
            Texture ("tile", "tile.png")
            Texture ("office", "office.png")
        ]
        resolution = Windowed (800, 800)
    }

    let tileSize = 32, 24
    let basey = 400

    let rectFor x y = 
        let halfw = (fst tileSize) / 2
        let posx = (x * halfw) + (y * halfw);
        let halfy = (snd tileSize) / 2
        let posy = (y * halfy) - (x * halfy)
        posx, basey + posy, fst tileSize, snd tileSize
    let colourFor = function "red" -> Color.Red | _ -> Color.Blue

    let advanceModel runState _ = 
        if wasJustPressed Keys.Escape runState then None else Some market
    
    let getView _ m =
        let tiles =
            [1..m.width] |> List.collect (fun x -> 
                [1..m.height] 
                |> List.where (fun y -> List.tryFind (fun o -> o.x = x && o.y = y) m.buildings = None)
                |> List.map (fun y ->
                    let rect = rectFor x y
                    match owner x y m with
                    | None -> Image ("tile", rect, Color.White)
                    | Some o -> Image ("tile", rect, colourFor o)
                ))
        let offices = m.buildings |> List.map (fun o -> 
            let (ox, oy, ow, oh) = rectFor o.x o.y
            let officeRect = ox, oy - ((snd tileSize) * 3), ow, oh * 4
            Image ("office", officeRect, colourFor o.owner))
        tiles @ offices

    runGame config advanceModel getView
    0
