module Reckless.Model

open Microsoft.Xna.Framework

type Corporation = {
    cash: int
    ideas: int
    headOffice: Office
    orders: Order list
    colour: Color
}
and Office = {
    x: int
    y: int
    managedOffices: Office list
    departments: Department list
    extensions: Extension list
}
and Department =
    | Product of quality: int 
    | Marketing
    | Research
    | Acquisitions
and Extension =
    | QA
and Order =
    | BuildDepartment of Office * Department
    | BuildExtension of Office * Extension
    | ResearchIdea of researchLocation:Office
    | BuildOffice of acquisitions:Office * x:int * y:int * Department