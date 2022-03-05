module Utilities

open Printf

exception UnexpectedError of string

let throw_formatted exnf fmt = ksprintf (fun s -> raise (exnf s)) fmt

let raise_error msg = throw_formatted UnexpectedError msg 
    