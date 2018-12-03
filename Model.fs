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
and Order = {
    office: Office option
    orderType: OrderType
} and OrderType =
    | BuildDepartment of Department
    | BuildExtension of Extension
    | ResearchIdea
    | BuildOffice of x:int * y:int * Department