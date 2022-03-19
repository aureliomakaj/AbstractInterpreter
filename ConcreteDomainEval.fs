module ConcreteDomainEval

open ConcreteDomain
open Utilities

let rec eval_expr (expr: expr) (state: mem_state) : value = 
    match expr with
    | Const c -> VSet (Set.singleton (c))
    | Var v -> 
        //Get all the values of the variable
        let values = List.filter (fun (var, _) -> v = var ) state
        VSet (Set.ofList (List.map (fun (_, n) -> n) values))
    
    | Range (NInt c1, NInt c2) ->
        if c1 > c2 then raise_error "Expression evaluation error: lower bound cannot be greater then higher bound"
        VSet (Set.ofList (List.map (fun x -> NInt x) [c1 .. c2]))

    | Neg ("-", e) ->
        let (VSet e) = eval_expr e state
        VSet (Set.map (fun (NInt x) -> NInt (-x)) e)
        
    | BinOp (e1, "+", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (+) eval_e1 eval_e2)

    | BinOp (e1, "-", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (-) eval_e1 eval_e2)

    | BinOp (e1, "*", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (*) eval_e1 eval_e2)

    | BinOp (e1, "/", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (/) eval_e1 (Set.filter (fun (NInt x) -> x <> 0 ) eval_e2))


    | _ -> raise_error "Expression evaluation error: expression not supported"

and interval_op op s1 s2 = 
    if s1.IsEmpty || s2.IsEmpty then 
        Set.empty
    else
        let operation n set = Set.map (fun (NInt n1) -> NInt ( (op) n n1)) set
        (Set.fold (fun acc (NInt n1) -> Set.union acc (operation n1 s2)) Set.empty s1)


let rec eval_cond cond states : mem_state = 
    match cond with 
    
    | Bool b -> 
        if b then 
            states
        else []

    | BoolOp (cond1, "and", cond2) ->
        let s1 = eval_cond cond1 states
        if  s1.IsEmpty
            then []
        else
            eval_cond cond2 s1

    | BoolOp (cond1, "or", cond2) ->
        let s1 = eval_cond cond1 states
        let s2 = eval_cond cond2 states
        s1 @ s2

    | NotOp c -> 
        match c with
        | Bool b0 -> eval_cond (Bool (not b0)) states
        
        | BoolOp (c1, "and", c2) -> eval_cond (BoolOp (NotOp c1, "or", NotOp c2) ) states
        | BoolOp (c1, "or", c2) -> eval_cond (BoolOp (NotOp c1, "and", NotOp c2) ) states  
        | Comparison (e1, "=", e2) -> eval_cond ( Comparison(e1, "<>", e2) ) states  
        | Comparison (e1, "<>", e2) -> eval_cond ( Comparison(e1, "=", e2) ) states  

        | Comparison (e1, ">", e2) -> eval_cond ( Comparison(e1, "<=", e2) ) states 
        | Comparison (e1, "<", e2) -> eval_cond ( Comparison(e1, ">=", e2) ) states 
        | Comparison (e1, ">=", e2) -> eval_cond ( Comparison(e1, "<", e2) ) states 
        | Comparison (e1, "<=", e2) -> eval_cond ( Comparison(e1, ">", e2) ) states 

    | Comparison (e1, "=", e2) -> comparition_op (=) e1 e2 states
    | Comparison (e1, "<>", e2) -> comparition_op (<>) e1 e2 states

    | Comparison (e1, ">", e2) -> comparition_op (>) e1 e2 states
    | Comparison (e1, "<", e2) -> comparition_op (<) e1 e2 states
    | Comparison (e1, ">=", e2) -> comparition_op (>=) e1 e2 states
    | Comparison (e1, "<=", e2) -> comparition_op (<=) e1 e2 states

    | _ -> raise_error "Conditional evaluation error: expression not supported"

and comparition_op op e1 e2 states = 
    let (VSet eval1) = eval_expr e1 states
    let (VSet eval2) = eval_expr e2 states
    let check v1 set = Set.exists (fun (NInt n) -> (op) v1 n) set
    let ok = Set.exists (fun (NInt x) -> check x eval2) eval1
    if ok then
        states
    else 
        []

let rec eval_prog prog states = 
    match prog with
    | Assign (var, expr) ->
        let (VSet eval) = eval_expr expr states
        let l = List.map (fun n -> (var, n)) (List.ofSeq eval)
        l @ (List.filter (fun (v, _ ) -> v <> var) states)

    | Seq (p1, p2) ->
        let s1 = eval_prog p1 states
        eval_prog p2 s1

    | IfThenElse (cond, p1, p2) ->
        let s1 = eval_prog p1 (eval_cond cond states)
        let s2 = 
            match p2 with
            | None -> []
            | Some pp2 -> eval_prog pp2 (eval_cond (NotOp cond) states)
        s1 @ s2

    | _ -> []
(*let rec eval_expr env expr = 
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
        if v2 = VInt 0 then raise_error "Detected division by 0"
        binary_operation (/) v1 v2

    | BinOp (e1, "%", e2) -> 
        let v1 = eval_expr env e1
        let v2 = eval_expr env e2
        if v2 = VInt 0 then raise_error "Detected division by 0"
        binary_operation (%) v1 v2

    | BinOp (e1, "==", e2) -> (==) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "!=", e2) -> (!=) (eval_expr env e1) (eval_expr env e2)

    | BinOp (e1, ">", e2) -> comparison (>) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "<", e2) -> comparison (<) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, ">=", e2) -> comparison (>=) (eval_expr env e1) (eval_expr env e2)
    | BinOp (e1, "<=", e2) -> comparison (<=) (eval_expr env e1) (eval_expr env e2)

    | UnOp ("-", expr) ->
        let value = eval_expr env expr
        match value with 
        | VInt n -> VInt -n
        | _ -> raise_error "Unary operator - implemented only of integers"

    | _ -> raise_error "Expression not implemented"

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
*) 