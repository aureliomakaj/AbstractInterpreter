module ConcreteDomainEval

open ConcreteDomain
open Utilities

//Expression evaluation
let rec eval_expr (expr: expr) (state: mem_state) : value = 
    match expr with
    //Constants
    | Const c -> VSet (Set.singleton (c))

    //Variables. The result depends on the environment
    | Var v -> 
        //Get all the values of the variable
        let values = List.filter (fun (var, _) -> v = var ) state
        VSet (Set.ofList (List.map (fun (_, n) -> n) values))
    
    //Interval
    | Range (NInt c1, NInt c2) ->
        if c1 > c2 then raise_error "Expression evaluation error: lower bound cannot be greater then higher bound"
        //Create a list with the given interval
        VSet (Set.ofList (List.map (fun x -> NInt x) [c1 .. c2]))

    //Negation
    | Neg ("-", e) ->
        let (VSet e) = eval_expr e state
        //Negate all the values the expression evaluates to
        VSet (Set.map (fun (NInt x) -> NInt (-x)) e)
    
    //Addition
    | BinOp (e1, "+", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (+) eval_e1 eval_e2)
    
    //Subtraction
    | BinOp (e1, "-", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (-) eval_e1 eval_e2)

    //Moltiplication
    | BinOp (e1, "*", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        VSet (interval_op (*) eval_e1 eval_e2)

    //Division
    | BinOp (e1, "/", e2) -> 
        let (VSet eval_e1) = eval_expr e1 state
        let (VSet eval_e2)= eval_expr e2 state
        //Apply the division but remove all the zeros from the denominator
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

    //Logical not
    | NotOp c -> 
        //Apply DeMorgan's laws and arithmetic's laws
        match c with
        | Bool b0 -> eval_cond (Bool (not b0)) states
        
        | BoolOp (c1, "and", c2) -> eval_cond (BoolOp (NotOp c1, "or", NotOp c2) ) states
        | BoolOp (c1, "or", c2) -> eval_cond (BoolOp (NotOp c1, "and", NotOp c2) ) states  
        | Comparison (e1, "=", e2) -> eval_cond ( Comparison(e1, "!=", e2) ) states  
        | Comparison (e1, "!=", e2) -> eval_cond ( Comparison(e1, "=", e2) ) states  

        | Comparison (e1, ">", e2) -> eval_cond ( Comparison(e1, "<=", e2) ) states 
        | Comparison (e1, "<", e2) -> eval_cond ( Comparison(e1, ">=", e2) ) states 
        | Comparison (e1, ">=", e2) -> eval_cond ( Comparison(e1, "<", e2) ) states 
        | Comparison (e1, "<=", e2) -> eval_cond ( Comparison(e1, ">", e2) ) states 

    | Comparison (e1, "=", e2) -> comparition_op (=) e1 e2 states
    | Comparison (e1, "!=", e2) -> comparition_op (<>) e1 e2 states

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

    | While (c, p) -> 
        let mutable s = eval_cond c states
        while not s.IsEmpty do
            s <- eval_cond c (eval_prog p s) 
        eval_cond (NotOp c) s
