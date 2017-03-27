(* Ocamllex scanner for MicroC *)

{ open Parser }

(* Complex regular expressions *)
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let string = '"' (ascii* as s) '"'
let whitespace = [' ' '\t' '\r']
(* NEW up to us how to define out nums. Doesn't allow exponential notation *)
let num = (digit+ '.' digit*) | ('.' digit+)
let int = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { multi_comment lexbuf }           (* Multiline comments *)
| "//"		{ single_comment lexbuf }	(* Single line comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ':'		{ COLON } (* NEW for our function syntax *)
| ','      { COMMA }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'		{ EXP }	(* NEW *)
| '_'		{ LOG } (* NEW *)
| '%'		{ MOD } (* NEW *)
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* Control flow, functions *)
| "def"		{ DEF } (* NEW for our function syntax *)
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data types *)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "string"	{ STRING } (* NEW *)
| "num"		{ NUM } (* NEW *)

(* Literals *)
| "true"   { TRUE }
| "false"  { FALSE }
| int as lxm { LITERAL(int_of_string lxm) }
| num as lxm { NUM_LITERAL(float_of_string lxm) }
| id as lxm { ID(lxm) }
| string	{ STRING_LITERAL(s) } (* NEW string literal *)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multi_comment = parse
  "*/" { token lexbuf }
| _    { multi_comment lexbuf }

and single_comment = parse
  '\n' { token lexbuf }
| _ {single_comment lexbuf }
