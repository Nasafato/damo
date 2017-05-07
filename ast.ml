(* Abstract Syntax Tree and functions for printing it *)

(* NEW mathematical operators *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Exp | Log | Mod

type uop = Neg | Not

(* NEW types *)
type typ = Int | Bool | Num | String | Symbol | Void

type lvalue = 
    Idl of string
  | ArrIdl of string * expr list

and expr =
    IntLit of int
  | BoolLit of bool
  | StringLit of string
  | NumLit of float
  | Id of string
  | ArrId of string * expr list
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of lvalue * expr
  | Call of string * expr list 
  | Noexpr

type bind = 
    Decl of typ * string
  | ArrDecl of typ * string * expr list

type stmt =
    Expr of expr
  | Block of stmt list 
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

(*type function_unit = 
    VarFunit of bind
  | StmtFunit of stmt
*)

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list; 
    body : program_sequence list;
  }

and program_sequence = 
    VarUnit of bind
  | FuncUnit of func_decl
  | StmtUnit of stmt

type program = program_sequence list 

(* Pretty-printing functions *)

(* NEW printing mathematical operators *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Exp -> "^"
  | Log -> "_"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

(* NEW printing strings with quotes *)
let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | NumLit(n) -> string_of_float n
  | StringLit(s) -> "\"" ^ s ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

(* NEW print string, print num *)
let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Num -> "num"
  | Symbol -> "symbol"

(*let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s*)
(*  | Bind(t, i) -> string_of_vdecl (t, i)

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_topstmts topstmts =
  "int main() { \n" ^
  String.concat "\n" (List.map string_of_stmt topstmts) ^ "}"

let string_of_program (topstmts, funcs) =
  String.concat "" (List.map string_of_fdecl funcs) ^ "\n" ^
  string_of_topstmts topstmts ^ "\n"
*)
