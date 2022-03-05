module Printer

open ConcreteDomain

let rec pretty_expr (e : expr) = 
    match e with
    | Int n -> sprintf "%d" n
    | Var v -> v
    | BinOp (e1, op, e2) -> sprintf "( %s %s %s )" (pretty_expr e1) op (pretty_expr e2)

let pretty_value value =
    match value with
    | VInt n -> sprintf "%d" n
    | VBool b -> sprintf "%b" b


let rec pretty_stm (stm: stm) = 
    match stm with
    | Assign (var, expr) -> sprintf "%s := %s;" var (pretty_expr expr)
    

    | IfThenElse (expr, stm1, stm2) ->
        sprintf "if %s\n  THEN\n{\n%s\n}\n  ELSE\n{\n%s\n}\n" (pretty_expr expr) (pretty_program stm1 ) (pretty_program stm2)

    | While (expr, stm1) -> 
        sprintf "while %s  DO \n {%s}" (pretty_expr expr) (pretty_program stm1 )
     

and pretty_program prog = List.fold (fun acc s -> acc + "\n" + (pretty_stm s)) "" prog
and pretty_env prog = List.fold (fun acc (s, v) -> acc + "\n" + (sprintf "%s = %s\n" s (pretty_value v))) "" prog
