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

    | IntrvInt n, PlusInf -> MinusInf // n - (+Inf) = -Inf
    | IntrvInt n, MinusInf -> PlusInf // n - (-Inf) = +Inf

    | MinusInf, IntrvInt n -> MinusInf // (-Inf) - n = -Inf
    | MinusInf, PlusInf -> MinusInf // (-Inf) - (+Inf) = -Inf

    | PlusInf, IntrvInt n -> PlusInf // (+Inf) - n = +Inf
    | PlusInf, MinusInf -> PlusInf // (+Inf) - (-Inf) = +Inf
    
    //Erros are (+Inf) - (+Inf) and (-Inf) - (-Inf)
    | _ -> raise_error "Abstraction error: minus operands not supported"

let number_prod num1 num2 = 
    match num1, num2 with
    | IntrvInt n1, IntrvInt n2 -> IntrvInt (n1 * n2) // n1 x n2 = n3

    | IntrvInt n, PlusInf 
    | PlusInf, IntrvInt n ->
        if n = 0 then 
            IntrvInt n      // 0 x (+Inf) = +Inf
        elif n < 0 then     
            MinusInf        // (-n) x (+Inf) = -Inf
        else
            PlusInf         // n x (+Inf) = +Inf
    
    | IntrvInt n, MinusInf -> MinusInf // n * (-Inf) = Inf

    | MinusInf, IntrvInt n -> MinusInf // (-Inf) - n = -Inf
    | MinusInf, PlusInf -> MinusInf // (-Inf) - (+Inf) = -Inf

    | PlusInf, MinusInf -> PlusInf // (+Inf) - (-Inf) = +Inf
    
    //Erros are (+Inf) - (+Inf) and (-Inf) - (-Inf)
    | _ -> raise_error "Abstraction error: product operands not supported"

let value_leq val1 val2 = 
    match val1, val2 with
    | Interval (l1, h1), Interval(l2, h2) -> (not (number_lower l1 l2)) && (number_leq h1 h2) // l1 >= l2 && h1 <= h2
    | Bottom, _ -> true
    | _, Bottom -> false

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
        let eval = eval_abstr_expr e state
        match eval with
        | Interval (n1, n2) ->
            match n1, n2 with
            | IntrvInt nat1, IntrvInt nat2 -> Interval (IntrvInt -nat2, IntrvInt -nat1)
            | MinusInf, IntrvInt nat1 -> Interval (IntrvInt -nat1, PlusInf)
            | IntrvInt nat1, PlusInf -> Interval (MinusInf, IntrvInt -nat1)
            | MinusInf, PlusInf
            | PlusInf, MinusInf -> Interval (MinusInf, PlusInf)
            | _ -> raise_error "Abstraction error: cannot have MinusInf as an upper bound"

        | Bottom -> Bottom
    | BinOp (e1, "+", e2) ->
        

    | _ -> raise_error "Abstraction error: expression not supported"

