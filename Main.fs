module Main

open System
open FSharp.Text.Lexing
open Parser
open Lexer
open ConcreteDomain
open Printer

let parse_prog (filename:string) = 
    printfn "loading source file '%s'..." filename
    use fstr = new IO.FileStream (filename, IO.FileMode.Open)
    use rd = new IO.StreamReader (fstr)
    //Buffer from stream 
    let lexbuf = LexBuffer<char>.FromTextReader rd
    let program = Parser.Prog Lexer.token lexbuf 
    match program with
    | Prog (l) -> print_program l


[<EntryPoint>]
let main argv =
    //try
    parse_prog argv.[0]
    //with ex -> printfn "%s" (ex.ToString())
    0 // return an integer exit code