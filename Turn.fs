module Turn

let endOfficeTurn office =
    ()

let endCorpTurn corp =
    // get buildings post order processing
    // end turn for each building to get cost and gains
    ()

let endTurn corps =
    corps |> Seq.map endCorpTurn
