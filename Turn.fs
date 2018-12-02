module Turn

let endOfficeTurn office =
    ()

let endCorpTurn corp =
    // get buildings post order processing
    // end turn for each building to get cost and gains
    ()

let endTurn corps =
    // advance corps
    // advance offices
    // gather all projected tiles
    // assign profits
    corps |> Seq.map endCorpTurn
