module Main.Orders

open Model

type Order = 
    {   name: string
        corpCondition: Corporation -> bool
        conditions: OrderCondition list
        action: OrderKind -> OrderAction list }
and OrderCondition =
    | OwnedOffice of (Office -> bool)
    | EnemyOffice of (Office -> bool)
    | Tile of ((int * int) -> bool)
and OrderKind =
    | OwnOffice of Office
    | OwnToOwnOffice of Office * Office
    | OwnToEnemyOffice of Office * Corporation * Office
    | OwnToTile of Office * (int * int)
    | Tile of (int * int)
and OrderAction = (Corporation * Office) -> (Corporation * Office)
and CorpOfficeIdentifier = string * (int * int)

// name
// action to run with typed variables
// conditions to evaluate
// name and condition/action?
// fun (ValidOffice) -> office
// could track order results rather than order actions...would allow precog of next turn...
// ValidOffice -> OwnOffice of (office -> bool) * (office -> office) | OtherOffice | Corp
// OrderAction instead of ValidOffice

let buildProductOrder = {
    name = "Build Product"
    corpCondition = fun corp -> corp.ideas > 0
    conditions = [
            OwnedOffice (fun o -> o.departments.Length < 6)
        ]
    action =
        function
        | OwnOffice o ->
            [
                fun (corp, office) -> 
                    if office.pos <> o.pos then corp, office
                    else 
                        { corp with ideas = corp.ideas - 1 }, 
                        { office with departments = Product 100::office.departments }
            ]
        | _ -> failwith "invalid parameters"
    }