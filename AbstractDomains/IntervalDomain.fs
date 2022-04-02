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


let eval_cond cond state = 
    match cond with
    | Comparison (e1, "<=", e2) ->
        let v1 = eval_abstr_expr e1 state
        let v2 = eval_abstr_expr e2 state
        match v1, v2 with
        | Interval (a, b), Interval (c, d) ->
            update_var ()
                
    | _ -> state