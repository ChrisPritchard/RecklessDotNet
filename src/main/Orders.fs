module Main.Orders

open Model

type Order = {
    name: string
    baseCondition: Corporation -> bool
    kind: OrderKind
} and OrderKind =
    | OfficeTarget of ValidOffice * (Office -> Office)
    | OfficeSourceAndTarget of ValidOffice * ValidOffice * (Office * Office -> Office)
and ValidOffice = Office -> bool

let buildProductOrder = {
    name = "Build Product"
    baseCondition = fun corp -> corp.ideas > 0
    kind = 
        let isValid office =
            office.departments.Length < 6
        let action office =
            { office with departments = Product 100::office.departments }
        OfficeTarget (isValid, action)
}

// TODO:
// base condition should check if order is valid in offices beneath executive, not whole corp?
// office condition should be able to check ownership - do we need a new member on office?
// action needs to be thought through - group orders by office and apply enmass?

let orderOptionsFor corporation executiveOffice = []