open Ast

(* main *)
let () =
  if Array.length Sys.argv < 2
    then print_endline "Please, give a file name" else
  let input_file = Sys.argv.(1) in
  if not (Sys.file_exists input_file)
    then print_endline "This file does not exist" else
  let lexbuf = Lexing.from_channel (open_in input_file) in
  let ast = Parser.main Lexer.token lexbuf in
  try Interpreter.run ast
  with EvaluationError e -> Printf.eprintf "Error: %s\n" e
