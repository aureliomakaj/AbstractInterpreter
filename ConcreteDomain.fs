module ConcreteDomain

//
type number = NInt of int

//Possible values an expression could evaluate to
type value = 
    | VSet of Set<number> //Set of numbers

//Expressions
type expr = 
    | Const of number //Constant
    | Var of string //Variable
    | Neg of string * expr //Unary operation
    | BinOp of expr * string * expr //Binary operation
    | Range of number * number //Interval

type cond = 
    | Bool of bool // True or False
    | BoolOp of cond * string * cond // Logic operations
    | NotOp of cond // Negation
    | Comparison of expr * string * expr // Expression comparison


//Statement
type prog = 
    | Skip 
    | Assign of string * expr //Assignment
    | Seq of prog * prog // Sequence of statements
    | IfThenElse of cond * prog * prog option // If - then - else
    | While of cond * prog // while loop
    

type mem_state = (string * number) list
