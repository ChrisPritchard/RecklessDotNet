module Main.Orders

open Model

type Order = 
    {   name: string
        components: OrderComponent list }
and OrderComponent =
    | CorpTransform of condition:(Corporation -> bool -> bool) * action:(Corporation -> Corporation)
    | OfficeTransform of condition:(Office -> bool -> bool) * action:(Office -> Office)

let buildProductOrder = {
    name = "Build Product"
    components = [
        CorpTransform (
            (fun corp isOwn -> isOwn && corp.ideas > 0), 
            fun corp -> { corp with ideas = corp.ideas - 1 })
        OfficeTransform (
            (fun office isOwn -> isOwn && office.departments.Length < 6), 
            fun office -> { office with departments = Product 100::office.departments })
    ]
}