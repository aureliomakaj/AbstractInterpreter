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

//Check if two intrv_number are equal
let number_eq num1 num2 = 
    match num1, num2 with

    | IntrvInt n1, IntrvInt n2 -> n1 = n2

    | PlusInf, PlusInf
    | MinusInf, MinusInf -> true

    | _ -> false

//Check if num1 is lower than num2
let number_lower num1 num2 = 
    match num1, num2 with

    | IntrvInt n1, IntrvInt n2 -> n1 < n2

    | PlusInf, PlusInf
    | MinusInf, MinusInf -> false

    | _, PlusInf
    | MinusInf, _ -> true

    | PlusInf, _
    | _, MinusInf -> false

//Check if num1 is lower or equal than num2
let number_leq num1 num2 = (number_lower num1 num2) || (number_eq num1 num2)

//Get the max between num1 and num2
let number_max num1 num2 = 
    if number_lower num1 num2 then
        num2
    else
        num1

//Get the min between num1 and num2
let number_min num1 num2 =
    if number_leq num1 num2 then 
        num1
    else
        num2

let number_sum num1 num2 = 
    match num1, num2 with
    | IntrvInt n1, IntrvInt n2 -> IntrvInt (n1 + n2)    // n1 + n2 = n3
    
    | IntrvInt n, PlusInf                               // n + (+Inf) = +Inf
    | PlusInf, IntrvInt n -> PlusInf                    // (+Inf) + n = +Inf

    | IntrvInt n, MinusInf                              // n + (-Inf) = -Inf
    | MinusInf, IntrvInt n -> MinusInf                  // (-Inf) + n = -Inf

    | PlusInf, PlusInf -> PlusInf                       // (+Inf) + (+Inf) = +Inf
    | MinusInf, MinusInf -> MinusInf                    // (-Inf) + (-Inf) = -Inf

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
            IntrvInt 0      // 0 x (+Inf) = 0
        elif n < 0 then     
            MinusInf        // (-n) x (+Inf) = -Inf
        else
            PlusInf         // n x (+Inf) = +Inf

    | IntrvInt n, MinusInf 
    | MinusInf, IntrvInt n ->
        if n = 0 then 
            IntrvInt 0      // 0 x (-Inf) = 0
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
    | Interval (a, b), Interval(c, d) -> (not (number_lower a c)) && (number_leq b d) // l1 >= l2 && h1 <= h2
    | Bottom, _ -> true
    | _, Bottom -> false

let value_neg intrv =
    match intrv with
    | Interval (n1, n2) ->
        match n1, n2 with
        | IntrvInt a, IntrvInt b -> Interval (IntrvInt -b, IntrvInt -a)                 // [a, b] = [-b, -a]
        | MinusInf, IntrvInt a -> Interval (IntrvInt -a, PlusInf)                       // [-Inf, n] = [-n, +Inf]
        | IntrvInt a, PlusInf -> Interval (MinusInf, IntrvInt -a)                       // [n, +Inf] = [-Inf, -n]                               
        | _ -> Interval (MinusInf, PlusInf)                                             // [+-Inf, +-Inf] = [-Inf, +Inf] Always sound
    | Bottom -> Bottom

// Union function. Bottom is neutral
let union val1 val2 = 
    match val1, val2 with 
    | Interval (a, b), Interval (c, d) -> Interval(number_min a c, number_max b d)
        
    | Bottom, _ -> val2
    | _, Bottom -> val1

//Intersection function. Bottom is absorbtive
let intersect val1 val2 = 
    match val1, val2 with 
    | Interval (a, b), Interval (c, d) ->
        let min = (number_max a c)
        let max = (number_min b d)
        if number_leq min max then
            Interval(min, max)
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

//Concretization function
let concretization abstr_value = 
    match abstr_value with
    | Interval (IntrvInt n1, IntrvInt n2) ->
        VSet (
            Set.ofList (
                List.map (fun x -> NInt x) [n1 .. n2]
            )
        )

    | _ -> VSet (Set.empty)

//Abstraction function
let abstraction value = 
    match value with
    | VSet set ->
        if set.IsEmpty 
            then Bottom
        else
            let (NInt min) = set.MinimumElement
            let (NInt max) = set.MaximumElement
            Interval (IntrvInt min, IntrvInt max)


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
        let higher = if b = PlusInf then d else b
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

let rec point_wise_intersection s1 s2 = 
    match s1, s2 with
    | _, []
    | [], _ -> []

    | (var, value) :: xs, ys -> 
        let found = List.tryFind (fun (v, _ ) -> v = var) ys

        //Make intersection between the abstract values
        let intersect_res = (
            match found with
            | None -> (var, Bottom)
            | Some (_, other_value) -> (var, intersect value other_value)
        )

        //If there exists an iteration that evaluate to bottom, then the total result is bottom
        match intersect_res with
        | (_, Bottom) -> []
        | (_, Interval _) ->
            
            //Filter the value found
            let filtered = 
                if found = None then 
                    ys //Since we found the value, we don't proceed to iterate the rest of the list
                else
                    List.filter( fun (v, _) -> v <> var ) ys

            match xs, filtered with
            | [], _ -> intersect_res :: filtered
            | _, [] -> [intersect_res]
            | _ -> 
                let forward_iteration = point_wise_intersection xs filtered
                if forward_iteration.IsEmpty then
                    []
                else
                    intersect_res :: forward_iteration
            

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


let rec eval_abstr_expr expr state = 
    match expr with
    | Const (NInt c) -> Interval(IntrvInt c, IntrvInt c)

    //Variables
    | Var v -> 
        //Get all the values of the variable
        let elem = List.tryFind (fun (var, _) -> v = var ) state
        match elem with
        | None -> Interval(MinusInf, PlusInf)//raise_error "Unknow variable %s" v
        | Some (_, value) -> value

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
        | Bool true -> []
        | Bool false -> state
        | Comparison (e1, "<=", e2) ->
            eval_abstr_cond (Comparison (e1, ">", e2)) state
        | Comparison (e1, ">", e2) ->
            eval_abstr_cond (Comparison (e1, "<=", e2)) state
        | _ -> state
    
    | BoolOp(c1, "and", c2) ->
        let res1 = eval_abstr_cond c1 state
        let res2 = eval_abstr_cond c2 state
        point_wise_intersection res1 res2

    | BoolOp(c1, "or", c2) ->
        let res1 = eval_abstr_cond c1 state
        let res2 = eval_abstr_cond c2 state
        point_wise_union res1 res2

    | Comparison (e1, ">", e2) ->
        match e1, e2 with
        | Var variable, Const _ ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_lower c b) then
                    update_var variable (Interval (number_max a (number_sum c (IntrvInt 1)), b)) state
                else
                    []
            | _ -> state

        | Var var_x, Var var_y ->
            let v1 = eval_abstr_expr e1 state
            let v2 = eval_abstr_expr e2 state
            match v1, v2 with
            | Interval (a, b), Interval (c, d) ->
                if (number_lower c b) then
                    let state_upd_x = update_var var_x (Interval ( (number_max a ( number_sum c (IntrvInt 1))), d)) state
                    update_var var_y (Interval (a, number_min b (number_minus d (IntrvInt 1)))) state_upd_x
                else
                    []
            | _ -> state
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
            | _ -> state

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
            | _ -> state
        | _ -> state
                
    | _ -> state


let rec check_abstrac_invariant curr_state next_state = 
    match curr_state, next_state with
    | [], [] -> true
    | _, [] -> false
    | [], _ -> false
    | (_, val_x) :: xs, (_, val_y) :: ys -> 
        if value_eq val_x val_y then
            check_abstrac_invariant xs ys
        else
            false

let rec eval_abstr_prog prog state state_points = 
    match state with
    | [] -> ([], state_points @ [[]])
    | _ ->
        match prog with
        //Assignment. If the expression is evaluated to Bottom, then we propagate it
        | Assign (var, expr) ->
            let v = eval_abstr_expr expr state
            let next = (
                match v with
                | Interval _ -> update_var var v state
                | Bottom -> []
            )

            (next, state_points @ [next])

        //Sequence of instruction. Execute the first and then the second on the states resulting from the previous one
        | Seq (p1, p2) ->
            let (s1, new_state_points) = eval_abstr_prog p1 state state_points
            eval_abstr_prog p2 s1 new_state_points

        | IfThenElse (cond, p1, p2) ->
            let eval_state_then = eval_abstr_cond cond state
            //Execute the body of the if on the states that satisfy the guard
            let (s1, _) = (
                match eval_state_then with
                | [] -> ([], [])
                | _ -> eval_abstr_prog p1 eval_state_then state_points
            )

            //If there is an else branch, execute the else-body on the states that do not satisfy the guard
            let (s2, _) = (
                match p2 with
                | None -> ([], [])
                | Some pp2 -> 
                    let eval_state_else = eval_abstr_cond (NotOp cond) state
                    match eval_state_else with 
                    | [] -> ([], [])
                    | _ -> eval_abstr_prog pp2 eval_state_else state_points
            )

            //The resulting set of states is the result of the point wise union
            let next = point_wise_union s1 s2
            (next, state_points @ [next])

        | While (c, p) -> 
            //The states satisfying the body
            let mutable s = eval_abstr_cond c state
            //Program states
            let mutable body_states = state_points
            //Temp variable used to check if we reached our fixpoint
            let mutable tmp = []
            let mutable invariant =  false
            while not invariant do
                //Evaluate the body on the states that satisfy the guard
                let (evalued_prg, prg_points) = eval_abstr_prog p (eval_abstr_cond c s) state_points
                //Save the program points
                body_states <- List.append body_states prg_points
                //Point wise unione between the original states and the last computed
                let union = point_wise_union state evalued_prg
                //Apply the widening to accelerate divergence
                tmp <- point_wise_widening s union
                //Check if we reached the invariant
                invariant <- check_abstrac_invariant s tmp
                //Save the new states
                s <- tmp

            let next = eval_abstr_cond (NotOp c) s
            let prg_points = body_states @ [next]
            (next, prg_points)
            //match c with
            //| Comparison (Var x, op, e2) ->
            //    let x_value = eval_abstr_expr (Var x) next
            //    let guard_value = eval_abstr_expr e2 next
            //    let final_state = update_var x (narrowing x_value guard_value) next
            //    (final_state, prg_points @ [final_state])
            //| _ -> (next, prg_points)

let rec set_init_vars prog = 
    match prog with
    
    | Assign (var, _) ->
        Set.singleton(var)
    
    | Seq (p1, p2) ->
        Set.union (set_init_vars p1) (set_init_vars p2)
    
    | IfThenElse (cond, p1, p2) ->
        Set.union (set_init_vars p1) (
            match p2 with
            | None -> Set.empty
            | Some pp2 -> set_init_vars pp2
        )
    
    | While (c, p) -> 
        set_init_vars p

let get_init_state prog = 
    let lhv = set_init_vars prog
    List.map (fun s -> (s, Interval(MinusInf, PlusInf))) (Set.toList lhv)