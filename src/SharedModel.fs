module SharedModel

open Xelmish.Model

type Corporation = {
    name: string
    abbreviation: string
    cash: int
    ideas: int
    headOffice: Office
    orders: Order list
    colour: Colour
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