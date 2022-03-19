module ConcreteDomain

//
type number = NInt of int

let negate n = -n

//Expressions
type expr = 
    | Const of number //Constant
    | Var of string //Variable
    | Neg of string * expr //Unary operation
    | BinOp of expr * string * expr //Binary operation
    | Range of number * number //Interval

type cond = 
    | Bool of bool
    | BoolOp of cond * string * cond
    | NotOp of cond
    | Comparison of expr * string * expr

//commands
type com = 
    | Assign of string * expr //Assignment
    | Cond of cond

//Possible values an expression could evaluate to
type value = 
    | VSet of Set<number> //Set of numbers

//Statement
type prog = 
    | Assign of string * expr //Assignment
    | Seq of prog * prog // Sequence of statements
    | IfThenElse of cond * prog * prog option // If - then - else
    | While of cond * prog // while loop
    
//Program
//type prog = Prog of stm list

type mem_state = (string * number) list

type env = mem_state list
//type coll_state = mem_state list
