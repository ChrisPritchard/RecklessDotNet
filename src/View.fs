module View

//open Xelmish.Model
//open Xelmish.Viewables
//open Constants
//open Helpers
//open Iso

//let getView runState gameState = 
//    let productTiles = gameProductTiles gameState
//    let mousePos = mouseTile runState
//    [
//        yield! renderMarket productTiles gameState
//        yield! renderOffices gameState

//        if gameState.selectedTile <> None then
//            yield! renderHighlight gameState (Option.get gameState.selectedTile)

//        if gameState.market.Contains mousePos then
//            yield! renderHighlight gameState mousePos

//        //match gameState.phase with
//        //| TurnEnding startTime ->
//        //    let amount = (runState.elapsed - startTime) / turnTransitionTime
//        //    yield Colour ((0, 0, winw, winh), new Color(0.f, 0.f, 0.f, float32 amount))
//        //| TurnStarting startTime ->
//        //    let amount = (runState.elapsed - startTime) / turnTransitionTime
//        //    yield Colour ((0, 0, winw, winh), new Color(0.f, 0.f, 0.f, 1.f - float32 amount))
//        //| _ -> ()
//    ]