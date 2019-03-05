module Model

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
and Colour = 
    | Red | Orange | Yellow | Green | Blue | Purple

type GameState = {
    market: Set<int * int>
    player: Corporation
    others: Corporation list
    selectedTile: (int * int) option
    phase: TurnPhase
} and TurnPhase =
    | Orders
    | ConfirmEndTurn
    | TurnEnding of startTime:float
    | TurnStarting of startTime:float
    
type UIModel = {
    endTurn: bool
    confirmOrders: bool
}