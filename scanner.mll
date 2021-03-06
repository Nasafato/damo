(* Ocamllex scanner for MicroC *)

{ open Parser 

(* TODO is this necessary? *)
let unescape s = 
	Scanf.sscanf ( "\"" ^s ^ "\"" ) "%S%!" (fun x -> x)
}

(* Complex regular expressions *) (* TODO are any of these unnecessary? *)
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ([' '-'!' '#'-'[' ']'-'~' ])
let digit = ['0'-'9']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let id = alpha (alpha | digit)*
let string_re = '"' ((ascii|escape)* as s) '"'
let whitespace = [' ' '\t' '\r']
let num_re = (digit+ '.' digit*) | ('.' digit+)
let int_re = digit+

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
| '['	   { LBRACKET }
| ']'	   { RBRACKET }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'		{ EXP }
| '_'		{ LOG }
| '%'		{ MOD }
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
| "def"		{ DEF }
| "if"     { IF }
| "else"   { ELSE }
| "elseif"	{ ELSEIF }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }

(* Data types *)
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "string"	{ STRING }
| "num"		{ NUM }
| "symbol"	{ SYMBOL }

(* Literals *) (* TODO can we replace all instances of Literal with Int_Literal? *)
| "true"   { TRUE }
| "false"  { FALSE }
| int_re as lxm { INT_LITERAL(int_of_string lxm) }
| num_re as lxm { NUM_LITERAL(float_of_string lxm) }
| string_re	{ STRING_LITERAL(unescape s) }

(* Others *)
| id as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multi_comment = parse
  "*/" { token lexbuf }
| _    { multi_comment lexbuf }

and single_comment = parse
  ['\n' '\r'] { token lexbuf }
| _ {single_comment lexbuf }
