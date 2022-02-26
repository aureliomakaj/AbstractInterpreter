module ConcreteDomain

//Expression
type expr = 
    | Int of int 
    | Var of string
    | BinOp of expr * string * expr 

//Statement
type stm = 
    | Assign of string * expr
    | IfThenElse of expr * statement * statement option
    | While of expr * statement

//Program
type prog = Prog of statement list