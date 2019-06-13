module Main.Orders

open Model

type Order = {
    name: string
    baseCondition: Corporation -> bool
    kind: OrderKind
} and OrderKind =
    | OfficeTarget of ValidOffice * (Office -> Corporation -> Office * Corporation)
    | OfficeSourceAndTarget of ValidOffice * ValidOffice * (Office -> Office -> Corporation -> Office * Corporation)
and ValidOffice = Office -> Market -> bool

let buildProductOrder = {
    name = "Build Product"
    baseCondition = fun corp -> corp.ideas > 0
    kind = 
        let isValid office market =
            List.exists (fun (o, _, _) -> o = office) market.player.allOffices
            && office.departments.Length < 6
        let action office corp =
            { office with departments = Product 100::office.departments },
            { corp with ideas = corp.ideas - 1 }
        OfficeTarget (isValid, action)
}

// TODO:
// base condition should check if order is valid in offices beneath executive, not whole corp?
// office condition should be able to check ownership - do we need a new member on office?
    // solved: add allOffices to corps, then pass market to valid office query
// action needs to be thought through - group orders by office and apply enmass?

let orderOptionsFor corporation executiveOffice = []