module Model

type Corporation = {
    cash: int
    ideas: int
    offices: Office list
    orders: Order list
}
and Office = {
    x: int
    y: int
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
    | ResearchIdea of Office
    | BuildLocation of x:int * y:int * Department