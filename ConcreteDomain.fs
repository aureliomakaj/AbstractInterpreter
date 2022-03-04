module ConcreteDomain


//Expression
type expr = 
    | Int of int 
    | Var of string
    | BinOp of expr * string * expr 

type value = 
    | VInt of int

//Statement
type stm = 
    | Assign of string * expr
    | IfThenElse of expr * stm * stm option
    | While of expr * stm

//Program
type prog = Prog of stm list

type env = (string * value) list

let rec eval_expr env expr = 
    match expr with
    | Int n -> n
    | Var v -> 
        let _, value = List.find (fun (var, _) -> var = v) env
        value

    | BinOp (e1, "+", e2) -> (eval_expr env e1) + (eval_expr env e2)
    | BinOp (e1, "-", e2) -> (eval_expr env e1) - (eval_expr env e2)
    | BinOp (e1, "*", e2) -> (eval_expr env e1) * (eval_expr env e2)
    | BinOp (e1, "/", e2) -> 
        let v1 = eval_expr env e1
        let v2 = eval_expr env e2
        if v2 = 0 then raise (Failure "Detected division by 0")
        v1 / v2

    | _ -> raise (Failure "Expression not implemented")

let rec eval_stm (env: env) (stm: stm) = 
    ()
    //match stm with
    //| Assign (var, expr) -> env <- (var, eval_expr env expr) :: env
    //| _ -> ()
