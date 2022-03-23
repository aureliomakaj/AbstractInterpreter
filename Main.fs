module Main

open Utilities
open System
open FSharp.Text.Lexing
open ConcreteDomain
open ConcreteDomainEval
open Printer

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
        let res = eval_prog program []
        List.iter (fun (var, NInt value) -> printf "%s -> %d; \n" var value) (List.rev res)


[<EntryPoint>]
let main argv =
    //try
    parse_prog argv.[0]
    //with ex -> printfn "%s" (ex.ToString())
    0 // return an integer exit code