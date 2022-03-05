module ConcreteDomain

//Expression
type expr = 
    | Int of int 
    | Var of string
    | BinOp of expr * string * expr 

type value = 
    | VInt of int
    | VBool of bool

//Statement
type stm = 
    | Assign of string * expr
    | IfThenElse of expr * stm list * stm list
    | While of expr * stm list

//Program
type prog = Prog of stm list

type env = (string * value) list

