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
        let (res, prg_points) = eval_abstr_prog program [] []
        //printf "%s" (pretty_env res)
        printf "PROGRAM POINTS ----------------------------\n"
        List.iter (fun env -> printf "%s------------------ \n" (pretty_env env)) prg_points


[<EntryPoint>]
let main argv =
    //try
    parse_prog argv.[0]
    //with ex -> printfn "%s" (ex.ToString())
    0 // return an integer exit code