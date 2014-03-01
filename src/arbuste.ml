open Ast
open Error

let parse file =
  let lexbuf = Lexing.from_channel (open_in file) in
  Parser.main Lexer.token lexbuf

(* main *)
let () =
  if Array.length Sys.argv < 2
    then print_endline "Please, give a file name" else
  let input_file = Sys.argv.(1) in
  if not (Sys.file_exists input_file)
    then print_endline "This file does not exist" else
  try
    let ast = parse input_file in
    Interpreter.run ast
  with ArbusteError e -> Error.print e
