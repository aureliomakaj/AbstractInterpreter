module Printer

open ConcreteDomain

let rec pretty_expr (e : expr) = 
    match e with
    | Int n -> sprintf "%d" n
    | Var v -> v
    | BinOp (e1, op, e2) -> sprintf "( %s %s %s )" (pretty_expr e1) op (pretty_expr e2)


let rec pretty_stm (stm: stm) = 
    match stm with
    | Assign (var, expr) -> sprintf "%s := %s" var (pretty_expr expr)
    
    | IfThenElse (expr, stm1, None) -> 
        sprintf "if %s\n  THEN\n    %s" (pretty_expr expr) (pretty_stm stm1)

    | IfThenElse (expr, stm1, Some stm2) -> 
        sprintf "if %s\n  THEN\n    %s\n  ELSE\n    %s" (pretty_expr expr) (pretty_stm stm1) (pretty_stm stm2)

    | While (expr, stm1) -> sprintf "while %s  DO \n %s" (pretty_expr expr) (pretty_stm stm1)


let print_program prog = List.iter (fun s -> printf "%s;\n" (pretty_stm s)) prog