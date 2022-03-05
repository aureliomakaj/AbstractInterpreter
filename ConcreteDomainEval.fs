module ConcreteDomainEval

open Utilities
open Printer
open ConcreteDomain

let rec eval_expr env expr = 
    match expr with
    | Int n -> VInt n
    | Var v -> 
        try
            let _, value = List.find (fun (var, _) -> var = v) env
            value
        with
        | :? System.Collections.Generic.KeyNotFoundException -> raise_error "Undefined variable %s" v
        
    | BinOp (e1, "+", e2) -> binary_operation (+) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "-", e2) -> binary_operation (-) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "*", e2) -> binary_operation (*) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "/", e2) -> 
        let v1 = eval_expr env e1
        let v2 = eval_expr env e2
        if v2 = VInt 0 then raise (Failure "Detected division by 0")
        binary_operation (/) v1 v2

    | BinOp (e1, "==", e2) -> (==) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "!=", e2) -> (!=) (eval_expr env e1) (eval_expr env e2)

    | BinOp (e1, ">", e2) -> comparison (>) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "<", e2) -> comparison (<) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, ">=", e2) -> comparison (>=) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "<=", e2) -> comparison (<=) (eval_expr env e1) (eval_expr env e2)

    | _ -> raise (Failure "Expression not implemented")

and binary_operation op e1 e2 = 
    match e1, e2 with 
    | VInt n1, VInt n2 -> VInt ((op) n1 n2)
    | _ -> raise_error "Unsupported operands for binary operation"

and comparison op e1 e2 = 
    match e1, e2 with 
    | VInt n1, VInt n2 -> VBool ((op) n1 n2)
    | _ -> raise_error "Unsupported operands for binary operation"


and (==) v1 v2 = 
    match v1, v2 with
    | VInt n1, VInt n2 -> VBool (n1 = n2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | _ -> raise_error "Unsupported operands for equality operation"

and (!=) v1 v2 = 
    match v1, v2 with
    | VInt n1, VInt n2 -> VBool (n1 <> n2)
    | VBool b1, VBool b2 -> VBool (b1 <> b2)
    | _ -> raise_error "Unsupported operands for equality operation"

and boolean_operation op e1 e2 =
    match e1, e2 with 
    | VBool b1, VBool b2 -> VBool ((op) b1 b2)
    | _ -> raise_error "Unsupported operands for boolean operation"


let rec eval_stm (env: env) (stm: stm) : env =
    match stm with
    | Assign (var, expr) -> 
        //Remove the variable from the environment if there is
        let filtered_env = List.filter (fun (s, _) -> s <> var) env
        //Evaluate the expression and save the variable in the environment
        (var, (eval_expr env expr)) :: filtered_env

        
    | IfThenElse (e1, stm_l1, stm_l2) -> 
        let value = eval_expr env e1
        match value with
        | VBool guard -> 
            if guard then
                eval_program env stm_l1
            else
                eval_program env stm_l2

        | _ -> raise_error "Expected boolean expression in if guard"

    | While (expr, stm_l) ->
        let value = eval_expr env expr
        match value with
        | VBool guard -> 
            if guard then
                let new_env = eval_program env stm_l
                eval_stm new_env stm
            else
                env

        | _ -> raise_error "Expected boolean expression in while guard"

and eval_program env prog = 
    match prog with
    | [] -> env
    | x :: xs -> 
        let new_env = eval_stm env x
        eval_program new_env xs
        