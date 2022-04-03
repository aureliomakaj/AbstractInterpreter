module AbstractDomains.IntervalDomain

open Utilities
open ConcreteDomain

type intrv_number = 
    | IntrvInt of int
    | PlusInf 
    | MinusInf


type intrv_value = 
    //For the Top, we use the interval [MinusInf, PlusInf]
    | Interval of intrv_number * intrv_number
    | Bottom

let number_eq num1 num2 = 
    match num1, num2 with

    | IntrvInt n1, IntrvInt n2 -> n1 = n2

    | PlusInf, PlusInf
    | MinusInf, MinusInf -> true

    | _ -> false

let number_lower num1 num2 = 
    match num1, num2 with

    | IntrvInt n1, IntrvInt n2 -> n1 < n2

    | PlusInf, PlusInf
    | MinusInf, MinusInf -> false

    | _, PlusInf
    | MinusInf, _ -> true

    | PlusInf, _
    | _, MinusInf -> false


let number_leq num1 num2 = (number_lower num1 num2) || (number_eq num1 num2)

let number_max num1 num2 = 
    if number_lower num1 num2 then
        num2
    else
        num1

let number_min num1 num2 =
    if number_leq num1 num2 then 
        num1
    else
        num2

let number_sum num1 num2 = 
    match num1, num2 with
    | IntrvInt n1, IntrvInt n2 -> IntrvInt (n1 + n2)    // n1 + n2 = n3
    
    | IntrvInt n, PlusInf               // n + (+Inf) = +Inf
    | PlusInf, IntrvInt n -> PlusInf    // (+Inf) + n = +Inf

    | IntrvInt n, MinusInf              // n + (-Inf) = -Inf
    | MinusInf, IntrvInt n -> MinusInf  // (-Inf) + n = -Inf

    | PlusInf, PlusInf -> PlusInf       // (+Inf) + (+Inf) = +Inf
    | MinusInf, MinusInf -> MinusInf    // (-Inf) + (-Inf) = -Inf

    | _ -> raise_error "Abstraction error: sum operands not supported"

let number_minus num1 num2 = 
    match num1, num2 with
    | IntrvInt n1, IntrvInt n2 -> IntrvInt (n1 - n2) // n1 - n2 = n3

    | IntrvInt n, PlusInf -> MinusInf               // n - (+Inf) = -Inf
    | IntrvInt n, MinusInf -> PlusInf               // n - (-Inf) = +Inf

    | MinusInf, IntrvInt n -> MinusInf              // (-Inf) - n = -Inf
    | MinusInf, PlusInf -> MinusInf                 // (-Inf) - (+Inf) = -Inf

    | PlusInf, IntrvInt n -> PlusInf                // (+Inf) - n = +Inf
    | PlusInf, MinusInf -> PlusInf                  // (+Inf) - (-Inf) = +Inf
    
    //Errors are (+Inf) - (+Inf) and (-Inf) - (-Inf)
    | _ -> raise_error "Abstraction error: minus operands not supported"

let number_prod num1 num2 = 
    match num1, num2 with
    | IntrvInt n1, IntrvInt n2 -> IntrvInt (n1 * n2) // n1 x n2 = n3

    | IntrvInt n, PlusInf 
    | PlusInf, IntrvInt n ->
        if n = 0 then 
            IntrvInt n      // 0 x (+Inf) = 0
        elif n < 0 then     
            MinusInf        // (-n) x (+Inf) = -Inf
        else
            PlusInf         // n x (+Inf) = +Inf

    | IntrvInt n, MinusInf 
    | MinusInf, IntrvInt n ->
        if n = 0 then 
            IntrvInt n      // 0 x (-Inf) = 0
        elif n < 0 then     
            PlusInf         // (-n) x (-Inf) = +Inf
        else
            MinusInf        // n x (-Inf) = -Inf
    
    | MinusInf, MinusInf -> PlusInf     // (-Inf) x (-Inf) = +Inf
    | PlusInf, PlusInf -> PlusInf       // (+Inf) x (+Inf) = +Inf

    | PlusInf, MinusInf
    | MinusInf, PlusInf -> MinusInf     // (-Inf) x (+Inf) = -Inf

let number_div num1 num2 =
    match num1, num2 with 
    | IntrvInt n1, IntrvInt n2 ->
        if n1 = 0 then
            num1                    // 0 / x = 0 (also for +-Inf)
        elif n2 = 0 then
            if n1 < 0 then 
                MinusInf            // (-c) / 0 = -Inf
            else 
                PlusInf             // (c) / 0 = +Inf
        else
            IntrvInt (n1 / n2)      // c1 / c2 = c3
    
    | IntrvInt _, PlusInf
    | IntrvInt _, MinusInf -> IntrvInt 0    // n / (+-Inf) = 0

    | PlusInf, IntrvInt n -> 
        if n >= 0 then
            PlusInf                         // (+Inf) / x = +Inf
        else        
            MinusInf                        // (+Inf) / (-x) = -Inf

    | MinusInf, IntrvInt n ->               
        if n >= 0 then
            MinusInf                        // (-Inf) / x = (-Inf)
        else
            PlusInf                         // (-Inf) / -x = (+Inf)

    | PlusInf, PlusInf
    | PlusInf, MinusInf
    | MinusInf, MinusInf
    | MinusInf, PlusInf -> IntrvInt 0       // (+-Inf) / (+-Inf) = 0

let value_eq val1 val2 = 
    match val1, val2 with
    | Interval (a, b), Interval (c, d) -> number_eq a c && number_eq b d
    | Bottom, Bottom -> true
    | _ -> false

let value_leq val1 val2 = 
    match val1, val2 with
    | Interval (l1, h1), Interval(l2, h2) -> (not (number_lower l1 l2)) && (number_leq h1 h2) // l1 >= l2 && h1 <= h2
    | Bottom, _ -> true
    | _, Bottom -> false

let value_neg intrv =
    match intrv with
    | Interval (n1, n2) ->
        match n1, n2 with
        | IntrvInt nat1, IntrvInt nat2 -> Interval (IntrvInt -nat2, IntrvInt -nat1)         // [a, b] = [-b, -a]
        | MinusInf, IntrvInt nat1 -> Interval (IntrvInt -nat1, PlusInf)                     // [-Inf, n] = [-n, +Inf]
        | IntrvInt nat1, PlusInf -> Interval (MinusInf, IntrvInt -nat1)                     // [n, +Inf] = [-Inf, -n]                               
        | _ -> Interval (MinusInf, PlusInf)                                                 // [+-Inf, +-Inf] = [-Inf, +Inf] Always sound
    | Bottom -> Bottom


let union val1 val2 = 
    match val1, val2 with 
    | Interval (l1, h1), Interval (l2, h2) -> Interval(number_min l1 l2, number_max h1 h2)
        
    | Bottom, _ -> val2
    | _, Bottom -> val1

let intersect val1 val2 = 
    match val1, val2 with 
    | Interval (l1, h1), Interval (l2, h2) ->
        let l = (number_max l1 l2)
        let h = (number_min h1 h2)
        if number_leq l h then
            Interval(l, h)
        else
            Bottom
        
    | Bottom, _
    | _, Bottom -> Bottom

let rec value_div v1 v2 = 
    match v1, v2 with
    | Interval (a, b), Interval (c, d) -> 
        if (number_eq c (IntrvInt 0)) && (number_eq d (IntrvInt 0)) then
            Bottom                                                          // Bottom if [c, d] = [0, 0]
        elif (number_leq (IntrvInt 0) c) then
            let ac = number_div a c
            let ad = number_div a d
            let bc = number_div b c
            let bd = number_div b d
            let min = number_min ac (number_min ad (number_min bc bd ))
            let max = number_max ac (number_max ad (number_max bc bd ))
            Interval (min, max)                                             // [min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)] if 0 <= c
        elif (number_leq d (IntrvInt 0)) then
            let t1 = value_neg v1
            let t2 = value_neg v2
            value_div t1 t2                                                 // [-b, -a] / [-d, -c] if d <= 0
        else
            let t1 = value_div v1 (Interval (c, IntrvInt 0))
            let t2 = value_div v1 (Interval (IntrvInt 0, d))
            union t1 t2                                                     // ([a, b] / [c, 0]) U ([a, b] / [0, d])

    | Bottom, _
    | _, Bottom -> Bottom

let concretization abstr_value = 
    match abstr_value with
    | Interval (IntrvInt n1, IntrvInt n2) ->
        VSet (
            Set.ofList (
                List.map (fun x -> NInt x) [n1 .. n2]
            )
        )

    | _ -> VSet (Set.empty)

let abstraction value = 
    match value with
    | VSet set ->
        if set.IsEmpty 
            then Bottom
        else
            let (NInt min) = set.MinimumElement
            let (NInt max) = set.MaximumElement
            Interval (IntrvInt min, IntrvInt max)

let rec eval_abstr_expr expr state = 
    match expr with
    | Const (NInt c) -> Interval(IntrvInt c, IntrvInt c)

    //Variables
    | Var v -> 
        //Get all the values of the variable
        let (_, value) = List.find (fun (var, _) -> v = var ) state
        value

    | Range (NInt n1, NInt n2) ->
        Interval(IntrvInt n1, IntrvInt n2)

    | Neg ("-", e) ->
        value_neg (eval_abstr_expr e state)

    | BinOp (e1, "+", e2) ->
        let v1 = eval_abstr_expr e1 state
        let v2 = eval_abstr_expr e2 state
        match v1, v2 with
        | Interval (a, b), Interval (c, d) -> Interval (number_sum a c, number_sum b d) // [a, b] + [c, d] = [a + c, b + d]
        | Bottom, _
        | _, Bottom -> Bottom

    | BinOp (e1, "-", e2) ->
        let v1 = eval_abstr_expr e1 state
        let v2 = eval_abstr_expr e2 state
        match v1, v2 with
        | Interval (a, b), Interval (c, d) -> Interval (number_minus a d, number_minus b c) // [a, b] - [c, d] = [a - d, b - c]
        | Bottom, _
        | _, Bottom -> Bottom

    | BinOp (e1, "*", e2) ->
        let v1 = eval_abstr_expr e1 state
        let v2 = eval_abstr_expr e2 state
        match v1, v2 with
        | Interval (a, b), Interval (c, d) -> // [a, b] x [c, d] = [min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
            let ac = number_prod a c
            let ad = number_prod a d
            let bc = number_prod b c
            let bd = number_prod b d
            let min = number_min ac (number_min ad (number_min bc bd ))
            let max = number_max ac (number_max ad (number_max bc bd ))
            Interval (min, max)
        | Bottom, _
        | _, Bottom -> Bottom

    | BinOp (e1, "/", e2) ->
        let v1 = eval_abstr_expr e1 state
        let v2 = eval_abstr_expr e2 state
        value_div v1 v2

    | _ -> Interval(MinusInf, PlusInf)

let update_var var value state = 
    let state_without_var = List.filter (fun (l, _) -> l <> var ) state
    (var, value) :: state_without_var


let rec eval_abstr_cond cond state = 
    match cond with
    | Bool true -> state
    | Bool false -> []

    | NotOp c ->
        match c with 
        | Comparison (e1, "<=", e2) ->
            eval_abstr_cond (Comparison (e1, ">", e2)) state
        | Comparison (e1, ">", e2) ->
            eval_abstr_cond (Comparison (e1, "<=", e2)) state
    
    | Comparison (e1, ">", e2) ->
        match e1, e2 with
        | Var variable, Const _ ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_leq c b) then
                    update_var variable (Interval (number_max a c, b)) state
                else
                    []
            | _ -> []

        | Var var_x, Var var_y ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_leq c b) then
                    let state_upd_x = update_var var_x (Interval (number_max a c, b)) state
                    update_var var_y (Interval (c, number_min b d)) state_upd_x
                else
                    []
            | _ -> []
        | _ -> state

    | Comparison (e1, "<=", e2) ->
        match e1, e2 with
        | Var variable, Const _ ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_leq a c) then
                    update_var variable (Interval (a, number_min b c)) state
                else
                    []
            | _ -> []

        | Var var_x, Var var_y ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_leq a d) then
                    let state_upd_x = update_var var_x (Interval (a, number_min b d)) state
                    update_var var_y (Interval (number_max a c, d)) state_upd_x
                else
                    []
            | _ -> []
        | _ -> state
                
    | _ -> state

let widening x y =
    match x, y with
    | Interval(a, b), Interval(c, d) -> 
        let lower = if number_leq a c then a else MinusInf
        let higher = if number_leq d b then b else PlusInf
        Interval (lower, higher)
    | Bottom, _ -> y
    | _, Bottom -> x

let narrowing x y = 
    match x, y with
    | Interval(a, b), Interval(c, d) -> 
        let lower = if a = MinusInf then c else a
        let higher = if d = PlusInf then b else d
        Interval (lower, higher)
    | Bottom, _ -> y
    | _, Bottom -> x

let rec point_wise_union s1 s2 = 
    match s1, s2 with
    | [], [] -> []
    | _, [] -> s1
    | [], _ -> s2
    | (var, value) :: xs, ys -> 
        let found = List.tryFind (fun (v, _ ) -> v = var) ys
        match found with
        | None -> (var, value) :: point_wise_union xs ys
        | Some (_, other_value) -> 
            let filtered = List.filter( fun (v, _) -> v <> var ) ys
            (var, union value other_value) :: point_wise_union xs filtered

let rec point_wise_widening s1 s2 = 
    match s1, s2 with
    | [], [] -> []
    | _, [] -> s1
    | [], _ -> s2
    | (var, value) :: xs, ys -> 
        let found = List.tryFind (fun (v, _ ) -> v = var) ys
        match found with
        | None -> (var, value) :: point_wise_widening xs ys
        | Some (_, other_value) -> 
            let filtered = List.filter( fun (v, _) -> v <> var ) ys
            (var, widening value other_value) :: point_wise_widening xs filtered

let rec check_abstrac_invariant curr_state next_state = 
    match curr_state, next_state with
    | [], [] -> true
    | _, [] -> false
    | (_, val_x) :: xs, (_, val_y) :: ys -> 
        if value_eq val_x val_y then
            check_abstrac_invariant xs ys
        else
            false

let rec eval_abstr_prog prog state = 
    match prog with

    | Assign (var, expr) ->
        let v = eval_abstr_expr expr state
        update_var var v state

    | Seq (p1, p2) ->
        let s1 = eval_abstr_prog p1 state
        eval_abstr_prog p2 s1

    | IfThenElse (cond, p1, p2) ->
        let s1 = eval_abstr_prog p1 (eval_abstr_cond cond state)
        let s2 = 
            match p2 with
            | None -> []
            | Some pp2 -> eval_abstr_prog pp2 (eval_abstr_cond (NotOp cond) state)
        point_wise_union s1 s2

    | While (c, p) -> 
        let mutable s = eval_abstr_cond c state
        let mutable tmp = []
        let mutable invariant =  false
        while not invariant do
            tmp <- point_wise_union state (eval_abstr_prog p (eval_abstr_cond c s))
            tmp <- point_wise_widening s tmp
            invariant <- check_abstrac_invariant s tmp
            s <- tmp

        eval_abstr_cond (NotOp c) s