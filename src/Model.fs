module Reckless.Model

open Microsoft.Xna.Framework
open GameCore.GameModel

type Corporation = {
    name: string
    abbreviation: string
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

type GameState = {
    map: Set<int * int>
    corps: Corporation list
    productTiles: Map<int * int, (Corporation * int) list>
    ui: ViewArtifact list
}