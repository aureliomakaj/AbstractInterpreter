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

