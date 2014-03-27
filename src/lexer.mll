{
  open Lexing
  open Parser

 (** Increments the lexing buffer line number counter.*)
  let increment_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = 0}
  
  (** Increments the lexing buffer line offset by the given length. *)
  let increment_bol lexbuf length =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- {pos with pos_bol = pos.pos_bol + length}

  (** Turns a char into a string containing this char. *)
  let string_of_char c = String.make 1 c
   
}

let blank = ['\t' '\r' ' ']
let newline = ('\n' | "\r\n")
let integer = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | newline {increment_linenum lexbuf; token lexbuf}
  | blank   {increment_bol lexbuf 1; token lexbuf}

  | "/*"  {increment_bol lexbuf 2; comment lexbuf}

  | '+' {increment_bol lexbuf 1; ADD}
  | '-' {increment_bol lexbuf 1; SUB}
  | '*' {increment_bol lexbuf 1; MUL}
  | '/' {increment_bol lexbuf 1; DIV}

  | '<' {increment_bol lexbuf 1; LESSER}
  | '>' {increment_bol lexbuf 1; GREATER}
  | '=' {increment_bol lexbuf 1; EQUAL}
  
  | "or"  {increment_bol lexbuf 2; OR}
  | "and" {increment_bol lexbuf 3; AND}

  | '(' {increment_bol lexbuf 1; BEGIN_PAR}
  | ')' {increment_bol lexbuf 1; END_PAR}
  
  | "print"  {increment_bol lexbuf 5; PRINT}
  | "read"   {increment_bol lexbuf 4; READ}
  | "stdout" {increment_bol lexbuf 6; STDOUT}
  | "stdin"  {increment_bol lexbuf 5; STDIN}
  
  | "seq"    {increment_bol lexbuf 3; SEQ}
  | "let"    {increment_bol lexbuf 3; LET}
  | "in"     {increment_bol lexbuf 2; IN}
  | "if"     {increment_bol lexbuf 2; IF}
  | "branch" {increment_bol lexbuf 6; BRANCH}

  | "func"      {increment_bol lexbuf 4; FUNC}
  | "param"     {increment_bol lexbuf 5; PARAM}
  | "eval"      {increment_bol lexbuf 4; EVAL}
  | "end_param" {increment_bol lexbuf 3; END_PARAM}

  | "true"  {BOOL true}
  | "false" {BOOL false}

  | "cast"   {CAST}
  | "string" {TO_STRING}
  | "int"    {TO_INT}
  | "bool"   {TO_BOOL}

  | integer as lxm  {increment_bol lexbuf (String.length lxm); INT (int_of_string lxm)}
  | ident as lxm    {increment_bol lexbuf (String.length lxm); IDENT lxm}

  | '"' {let buffer = Buffer.create 20 in string_value buffer lexbuf}

  | eof {EOF}

  | _  as lxm {Error.raise_positioned ("Unrecognized character " ^ (string_of_char lxm)) lexbuf.lex_curr_p}

and string_value buffer = parse
  | '"' {STRING (Buffer.contents buffer)}
  | eof {Error.raise_positioned "Unexpected end of file in a open string" lexbuf.lex_curr_p}

  | "\\t"     { Buffer.add_char buffer '\t'; string_value buffer lexbuf }
  | "\\n"     { Buffer.add_char buffer '\n'; string_value buffer lexbuf }
  | '\\' '"'  { Buffer.add_char buffer '"'; string_value buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; string_value buffer lexbuf }
  
  | _ as c {Buffer.add_char buffer c; string_value buffer lexbuf}

and comment = parse
  | "*/" {increment_bol lexbuf 2; token lexbuf}
  | _    {increment_bol lexbuf 1; comment lexbuf}
  | eof  {Error.raise_positioned "Unexpected end of file in a comment" lexbuf.lex_curr_p}

{}
