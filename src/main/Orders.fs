module Main.Orders

open Model

type Order = 
    {   name: string
        components: (OrderSelector * OrderTransform) list }
and OrderSelector =
    | OfficeSelector of (Office -> bool -> bool)
    | CorpSelector of (Corporation -> bool -> bool)
    | Tile of ((int * int) -> bool)
and OrderTransform =
    | OfficeTransform of (Office -> Office)
    | CorpTransform of (Corporation -> Corporation)

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
    components = [
        CorpSelector (fun corp isOwn -> isOwn && corp.ideas > 0), 
        CorpTransform (fun corp -> { corp with ideas = corp.ideas - 1 })
        OfficeSelector (fun office isOwn -> isOwn && office.departments.Length < 6), 
        OfficeTransform (fun office -> { office with departments = Product 100::office.departments })
    ]
}