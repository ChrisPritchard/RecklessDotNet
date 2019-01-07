
open Model
open Turn

[<EntryPoint>]
let main _ =

    let test = {
        x = 10
        y = 10
        managedOffices = []
        departments = [Product 16; Marketing; Product 22; Product 30]
        extensions = []
    }

    updateQuality test false |> fun o -> printfn "%A" o.departments
    


    0
