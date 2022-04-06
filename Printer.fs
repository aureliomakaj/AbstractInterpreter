module Printer

open AbstractDomains.IntervalDomain
open ConcreteDomain

let pretty_abstract_number number = 
    match number with
    | IntrvInt a -> sprintf "%d" a
    | PlusInf -> "+Inf"
    | MinusInf -> "-Inf"

let pretty_abstract_value value = 
    match value with
    | Interval (a, b) -> sprintf "[%s, %s]" (pretty_abstract_number a) (pretty_abstract_number b)
    | Bottom -> sprintf "Bottom"

let pretty_env env =
    let res = List.fold (fun acc (var, value) -> acc + sprintf "%s -> %s; \n" var (pretty_abstract_value value)) "" (List.rev env)
    if res.Length = 0 then 
        "Bottom\n"
    else
        res