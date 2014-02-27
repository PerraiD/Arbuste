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

  (** Removes the quotes from the given string. *)
  let remove_quotes s =
    let length = String.length s in
    String.sub s 1 (length - 2)
}

let blank = ['\t' '\r' ' ']
let newline = ('\n' | "\r\n")
let integer = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
let string_value = '"'[^'"']*'"'

rule token = parse
  | newline {increment_linenum lexbuf; token lexbuf}
  | blank   {increment_bol lexbuf 1; token lexbuf}

  | "/*"  {increment_bol lexbuf 2; comment lexbuf}

  | '+' {increment_bol lexbuf 1; ADD}
  | '-' {increment_bol lexbuf 1; SUB}
  | '*' {increment_bol lexbuf 1; MUL}
  | '/' {increment_bol lexbuf 1; DIV}

  | '(' {increment_bol lexbuf 1; BEGIN_PAR}
  | ')' {increment_bol lexbuf 1; END_PAR}
  
  | "print"  {increment_bol lexbuf 5; PRINT}
  | "stdout" {increment_bol lexbuf 6; STDOUT}
  
  | "seq" {increment_bol lexbuf 3; SEQ}
  | "let" {increment_bol lexbuf 3; LET}
  | "in"  {increment_bol lexbuf 2; IN}

  | integer as lxm      {increment_bol lexbuf (String.length lxm); INT(int_of_string lxm)}
  | ident as lxm        {increment_bol lexbuf (String.length lxm); IDENT(lxm)}
  | string_value as lxm {increment_bol lexbuf (String.length lxm); STRING(remove_quotes lxm)}

  | eof {EOF}

  | _  as lxm {Error.unrecognized_char lexbuf.lex_curr_p lxm; token lexbuf}

and comment = parse
  | "*/" {increment_bol lexbuf 2; token lexbuf}
  | _    {increment_bol lexbuf 1; comment lexbuf}
  | eof  {EOF}

{}
