open Ast 

type t = 
    Int
  | Bool
  | Num 
  | String 
  | Symbol
  | Void

type lvalue = 
    Idl of t *string
  | ArrIdl of t * string * s_expr list

and s_expr =
    IntLit of t * int
  | BoolLit of t * bool
  | StringLit of t * string
  | NumLit of t * float
  | Id of t * string
  | ArrId of t * string * s_expr list
  | Binop of t * s_expr * Ast.op * s_expr
  | Unop of t * Ast.uop * s_expr
  | Assign of lvalue * s_expr
  | Call of t * string * s_expr list
  | Noexpr of t


type s_bind = 
    Decl of t * string
  | ArrDecl of t * string * s_expr list


type s_stmt =
    Expr of s_expr
  | Block of s_stmt list
  | Return of s_expr
  | If of s_expr * s_stmt * s_stmt
  | For of s_expr * s_expr * s_expr * s_stmt
  | While of s_expr * s_stmt


type s_function_unit = 
    VarFunit of s_bind
  | StmtFunit of s_stmt
 
type s_func_decl = {
    s_typ : t;
    s_fname : string;
    s_formals : s_bind list; 
    s_body : s_function_unit list;
  }
 
type s_program_unit = 
    VarUnit of s_bind
  | FuncUnit of s_func_decl
  | StmtUnit of s_stmt

type s_program = s_program_unit list 

