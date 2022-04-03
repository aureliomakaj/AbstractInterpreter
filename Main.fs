module Main

open Utilities
open System
open FSharp.Text.Lexing
open ConcreteDomain
open ConcreteDomainEval
open Printer
open AbstractDomains.IntervalDomain

let trap f =
    try f ()
    with 
       | UnexpectedError msg       -> printfn "\nunexpected error: %s" msg

let parse_prog (filename:string) = 
    trap <| fun () ->
        printfn "loading source file '%s'..." filename
        use fstr = new IO.FileStream (filename, IO.FileMode.Open)
        use rd = new IO.StreamReader (fstr)
        //Buffer from stream 
        let lexbuf = LexBuffer<char>.FromTextReader rd
        let program = Parser.prog Lexer.token lexbuf
        let res = eval_abstr_prog program []
        List.iter (fun (var, value) -> printf "%s -> %s; \n" var (pretty_abstract_value value)) (List.rev res)


[<EntryPoint>]
let main argv =
    //try
    parse_prog argv.[0]
    //with ex -> printfn "%s" (ex.ToString())
    0 // return an integer exit code