module ConcreteDomain

//Expression
type expr = 
    | Int of int 
    | Var of string
    | BinOp of expr * string * expr 

//Statement
type stm = 
    | Assign of string * expr
    | IfThenElse of expr * stm * stm option
    | While of expr * stm

//Program
type prog = Prog of stm list

    
