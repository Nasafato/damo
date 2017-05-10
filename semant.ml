(* Semantic checking for the damo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
(*type env_var = {v_type: Sast.t; v_expr: Sast.expr; v_name: string}
type env_global = {sym_table: env_var StringMap.t;}
type env_function = {env_name: string; sym_table: env_var StringMap.t; env_ret_type: Sast.t}
*)
let function_map = Hashtbl.create 10;;
  (*maps for function name and type info, such as return type and length of paramter list-type checking of actuals done at runtime*)
let function_map_type = Hashtbl.create 10;;
let function_map_length = Hashtbl.create 10;; 
let function_map_formals = Hashtbl.create 10;;
let global_scope = Hashtbl.create 15;;

let convert program_list =
  (*map for function name and list of variables, including formals and locals*)
  let convert_type_tuple x = let (t, _) = x in match t with  
        Ast.Int    -> Sast.Int
      | Ast.Bool   -> Sast.Bool
      | Ast.Num    -> Sast.Num 
      | Ast.Symbol -> Sast.Symbol
      | Ast.Void   -> Sast.Void
      | Ast.String -> Sast.String
       
  in

  let convert_type = function 
	Ast.Int    -> Sast.Int
      | Ast.Bool   -> Sast.Bool
      | Ast.Num    -> Sast.Num 
      | Ast.Symbol -> Sast.Symbol
      | Ast.Void   -> Sast.Void
      | Ast.String -> Sast.String
 
  in
 
  let extract_type s_expr = match s_expr with 
	Sast.Id(t, _)             -> t
      | Sast.Noexpr(t)            -> t 
      | Sast.Binop(t, _, _ ,_)    -> t
      | Sast.IntLit(t, _)         -> t
      | Sast.BoolLit(t, _)        -> t
      | Sast.StringLit(t, _)      -> t
      | Sast.NumLit(t, _)         -> t
      | Sast.ArrId(t, _, _)       -> t
      | Sast.Unop(t, _, _)        -> t 
      | Sast.Call(t, _, _)        -> t
      | _                         -> raise(Failure("cannot extract type for this"))
  in
 
  (*Hashtbl.add global_scope "test" (Ast.String, 0);*)
  
  Hashtbl.add function_map_formals "print" [(Ast.String, "x")];
  Hashtbl.add function_map_formals "print_int" [(Ast.Int, "x")];
  Hashtbl.add function_map_formals "print_bool" [(Ast.Bool, "x")];
  Hashtbl.add function_map_formals "print_num" [(Ast.Num, "x")];
  Hashtbl.add function_map_formals "left" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "right" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "operator" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "isInitialized" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "value" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "strcompare" [(Ast.String, "x"); (Ast.String, "y")];
  Hashtbl.add function_map_formals "isConstant" [(Ast.Symbol, "x")];
  Hashtbl.add function_map_formals "operator" [(Ast.Symbol, "x")];

  Hashtbl.add function_map_length "print" 1; 
  Hashtbl.add function_map_length "print_int" 1;
  Hashtbl.add function_map_length "print_bool" 1;
  Hashtbl.add function_map_length "print_num" 1;
  Hashtbl.add function_map_length "left" 1;
  Hashtbl.add function_map_length "right" 1;
  Hashtbl.add function_map_length "operator" 1;
  Hashtbl.add function_map_length "isInitialized" 1;
  Hashtbl.add function_map_length "value" 1;
  Hashtbl.add function_map_length "strcompare" 2;
  Hashtbl.add function_map_length "operator" 1;
  Hashtbl.add function_map_length "isConstant" 1;

  Hashtbl.add function_map_type "print" Ast.Void;
  Hashtbl.add function_map_type "print_int" Ast.Void;
  Hashtbl.add function_map_type "print_bool" Ast.Void;
  Hashtbl.add function_map_type "print_num" Ast.Void;
  Hashtbl.add function_map_type "left" Ast.Symbol;
  Hashtbl.add function_map_type "right" Ast.Symbol;
  Hashtbl.add function_map_type "operator" Ast.String;
  Hashtbl.add function_map_type "isInitialized" Ast.Int;
  Hashtbl.add function_map_type "value" Ast.Num;
  Hashtbl.add function_map_type "strcompare" Ast.Int;
  Hashtbl.add function_map_type "isConstant" Ast.Int;
  Hashtbl.add function_map_type "operator" Ast.String;

  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.String, 0); Hashtbl.add function_map "print" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Int, 0); Hashtbl.add function_map "print_int" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Bool, 0); Hashtbl.add function_map "print_bool" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Num, 0); Hashtbl.add function_map "print_num" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "left" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "right" new_map;
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "isConstant" new_map; 
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "value" new_map;
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.String, 0); Hashtbl.add new_map "y" (Ast.String, 0); Hashtbl.add function_map "strcompare" new_map;
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "operator" new_map;
  let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" (Ast.Symbol, 0); Hashtbl.add function_map "isInitialized" new_map; 



(*  let printing_functions = ["print" ; "print_int" ; "print_num" ; "print_bool"] in
  let symbol_functions = ["left" ; "right" ; "operator" ; "isConstant" ] in
  let built_in_functions = printing_functions @ symbol_functions in
  let add_builtin fname = match fname with 
        "print" -> Hashtbl.add function_map_formals fname [(Ast.String, "x")]; 
		   let new_map = Hashtbl.create 10;
		   Hashtbl.add new_map "x" Ast.String;
		   Hashtbl.add function_map fname new_map; 
		   Hashtbl.add function_map_length fname 1; 
		   Hashtbl.add function_map_type fname (Ast.Void)
      | "print_int" -> 
		ignore(Hashtbl.add function_map_formals fname [(Ast.Int, "x")]); 
		ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Int) in Hashtbl.add function_map fname new_map; 
		ignore(Hashtbl.add function_map_length fname 1); 
		Hashtbl.add function_map_type fname (Ast.Void)

      | "print_num" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Num, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Num) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.Void)

      | "print_bool" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Bool, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Bool) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.Void)

      | "left" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Symbol, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Symbol) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.Symbol) 

      | "right" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Symbol, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Symbol) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.Symbol)

      | "operator" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Symbol, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Symbol) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.String)

      | "isConstant" -> ignore(Hashtbl.add function_map_formals fname [(Ast.Symbol, "x")]); ignore(let new_map = Hashtbl.create 10 in Hashtbl.add new_map "x" Ast.Symbol) in Hashtbl.add function_map fname new_map; ignore(Hashtbl.add function_map_length fname 1); Hashtbl.add function_map_type fname (Ast.Bool)

  in List.iter (fun x -> add_builtin x) built_in_functions in 
  *)   
  let extract_type_lvalue lvalue = match lvalue with 
        Sast.Idl(t, _)            -> t
      | Sast.ArrIdl(t, _, _)      -> t
  in 

  let check_assign lvaluet rvaluet err = match lvaluet, rvaluet with
      (Sast.Symbol, Sast.Int) -> Sast.Symbol
    | (Sast.Symbol, Sast.Num) -> Sast.Symbol
    | (Sast.Num, Sast.Int) -> Sast.Num
    | (_, _) -> if lvaluet = rvaluet then lvaluet else raise err in

  (* global scope record for all global variables *) 
   
  let type_of_identifier s env =
      
      try Hashtbl.find env s
      with Not_found -> try Hashtbl.find global_scope s with Not_found -> raise (Failure("undeclared identifier " ^s))
       
  in
  
  let find_func fname =  
      try Hashtbl.find function_map fname
      with Not_found -> raise (Failure ("undeclared function"))
  in 
  
  let rec check_expr_legal e env = let e' = expr env e in if extract_type e' = Sast.Int then e' else raise(Failure ("Incorrect type")) 
  
  
  and check_lvalue env lvalue  = match lvalue with  
        Ast.Idl(s) -> Sast.Idl(convert_type_tuple(type_of_identifier s env), s)
      | Ast.ArrIdl(s, el) -> List.iter (fun a -> ignore(check_expr_legal a env)) el; let el' = List.map (fun a -> expr env a) el in Sast.ArrIdl(convert_type_tuple(type_of_identifier s env), s, el')
 
  and expr env e = match e with 
        Ast.IntLit(i)    -> Sast.IntLit(Sast.Int, i)
      | Ast.BoolLit(b)   -> Sast.BoolLit(Sast.Bool, b)
      (* NEW an expression can be a string literal *)
      | Ast.NumLit(n)    -> Sast.NumLit(Sast.Num, n)
      | Ast.StringLit(s) -> Sast.StringLit(Sast.String, s)
      | Ast.Id(s) -> Sast.Id(convert_type_tuple (type_of_identifier s env), s)
      | Ast.ArrId(a, el) -> List.iter (fun x -> ignore(check_expr_legal x env)) el; let el' = List.map(fun x -> expr env x) el in  Sast.ArrId(convert_type_tuple (type_of_identifier a env), a, el')
      | Ast.Binop(e1, op, e2) -> let e1' = expr env e1 and e2' = expr env e2 in let t1 = extract_type e1' and t2 = extract_type e2' in
      	(match op with
              Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Int && t2 = Sast.Num )-> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Num && t2 = Sast.Int) -> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Int && t2 = Sast.Int -> Sast.Binop(Sast.Int, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Num && t2 = Sast.Num -> Sast.Binop(Sast.Num, e1', op, e2')
            (* NEW allow symbols to be operated on with nums *)
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Exp | Ast.Log  when t1 = Sast.Symbol && t2 = Sast.Num -> Sast.Binop(Sast.Symbol, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Exp | Ast.Log  when t1 = Sast.Num && t2 = Sast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Exp | Ast.Log  when t1 = Sast.Symbol && t2 = Sast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')
             | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Exp | Ast.Log  when t1 = Sast.Symbol && t2 = Sast.Int -> Sast.Binop(Sast.Symbol, e1', op, e2')
              | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Exp | Ast.Log  when t1 = Sast.Int && t2 = Sast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')
            (* NEW only allow for comparison of ints and nums *)
          	| Ast.Equal | Ast.Neq when t1 = t2 && (t1 = Sast.Int || t1 = Sast.Num) -> Sast.Binop(Sast.Bool, e1', op, e2')
          	| Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq when t1 = Sast.Int && t2 = Sast.Int -> Sast.Binop(Sast.Bool, e1', op, e2')
                | Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq when t1 = Sast.Num && t2 = Sast.Num -> Sast.Binop(Sast.Bool, e1', op, e2')
 		| Ast.Equal when t1 = Sast.Symbol && t2 = Sast.Symbol -> Sast.Binop(Sast.Bool, e1', op, e2')

          	| Ast.And | Ast.Or when t1 = Sast.Bool && t2 = Sast.Bool -> Sast.Binop(Sast.Bool, e1', op, e2')
                  | _ -> raise (Failure ("illegal binary operator "))
        )
      | Ast.Unop(op, e) -> let e' = expr env e in let t = extract_type e' in
        (match op with
           Ast.Neg when t = Sast.Int -> Sast.Unop(Sast.Int, op, e')
         | Ast.Not when t = Sast.Bool -> Sast.Unop(Sast.Bool, op, e')
         | _ -> raise (Failure ("illegal unary operator ")))
      | Ast.Noexpr -> Sast.Noexpr(Sast.Void)
      | Ast.Assign(lvalue, e) -> let new_lvalue = check_lvalue env lvalue in let lt = extract_type_lvalue new_lvalue and rt = extract_type (expr env e) in ignore(check_assign lt rt (Failure ("illegal assignment "))); Sast.Assign(new_lvalue, expr env e)
      | Ast.Call(fname, actuals) -> ignore(find_func fname); if List.length actuals != Hashtbl.find function_map_length fname then
          raise (Failure ("incorrect number of arguments"))
        else 
	  let new_actuals = List.map (fun e -> expr env e) actuals in
          List.iter2 (fun (ft, _) e -> let et = extract_type e in
                       ignore (check_assign (convert_type ft) et
                                 (Failure ("illegal actual argument found, wrong type"))))
            (try Hashtbl.find function_map_formals fname with 
             | Not_found -> raise(Failure("function not defined in formals map"))) new_actuals;
        Sast.Call(convert_type (try Hashtbl.find function_map_type fname with Not_found -> raise(Failure("function not defined in types map"))), fname, new_actuals)
    
  in 


  let report_duplicate_map var env = 
    if Hashtbl.mem env var = false then true else raise(Failure("duplicate found")); 
  
  in 
  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in
    
  (* Raise an exception if a given binding is to a void type *)
  let check_not_void t = match t with 
      Ast.Void -> raise (Failure ("can't have void variable"))  
    | _ -> ()
  in


  let check_not_void_map _ argTwo  = match argTwo with 
      (Ast.Void, _) -> raise(Failure("cannot have void variable type"))
    | (_, _) -> ()
  in
 
    let check_vdecl = function
      Ast.Decl(t,name) ->  ignore(report_duplicate_map name global_scope); ignore(Hashtbl.add global_scope name (t, 0)); Hashtbl.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; Sast.Decl(convert_type t, name)
    | Ast.ArrDecl(t, n, l) -> ignore(report_duplicate_map n global_scope); ignore(Hashtbl.add global_scope n (t, List.length l)); Hashtbl.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; List.iter (fun a -> ignore(check_expr_legal a global_scope)) l; let l' = List.map (fun a -> expr global_scope a ) l in Sast.ArrDecl(convert_type t, n, l')    
        
  in
  
  let check_vdecl_function function_name function_line = match function_line with  
 	Ast.Decl(t,name) -> let f_map = (try Hashtbl.find function_map function_name with Not_found -> raise(Failure("function not in function_map"))) in ignore(report_duplicate_map name f_map); ignore(report_duplicate_map name global_scope); ignore(Hashtbl.add f_map name (t, 0)); Hashtbl.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; Sast.Decl(convert_type t, name)
    | Ast.ArrDecl(t, n, l) -> let f_map = (try Hashtbl.find function_map function_name with Not_found -> raise(Failure("function not in function map"))) in ignore(report_duplicate_map n f_map); ignore(report_duplicate_map n global_scope); ignore(Hashtbl.add f_map n (t, List.length l)); Hashtbl.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; List.iter (fun a -> ignore(check_expr_legal a f_map)) l; let l' = List.map (fun a -> expr f_map a) l in Sast.ArrDecl(convert_type t, n, l') 
		
  in
  
  let check_bool_expr env e = if extract_type (expr env e) <> Sast.Bool
      then raise (Failure ("expected boolean expression "))
      else () in

    (* verify a statement or throw an exception *)
  let rec stmt env fname s = match s with 
        Ast.Expr(e) -> Sast.Expr(expr env e)
      | Ast.Block(sl) -> let sl' = List.map (fun a -> stmt env fname a) sl in Sast.Block(sl')
      | Ast.Return(e) -> if fname = "" then raise(Failure("can't have return type outside of function")) else let t = extract_type (expr env e) in if t = convert_type (try Hashtbl.find function_map_type fname with Not_found -> raise(Failure("function not in function_map_type"))) then Sast.Return(expr env e) else raise (Failure ("incorrect return type"))
      | Ast.If(p, b1, b2) -> check_bool_expr env p; Sast.If(expr env p, stmt env fname b1, stmt env fname b2)
      | Ast.For(e1, e2, e3, st) -> check_bool_expr env e2; Sast.For(expr env e1, expr env e2, expr env e3, stmt env fname st)
      | Ast.While(p, s) -> check_bool_expr env p; Sast.While(expr env p, stmt env fname s)
   
  in
  let check_stmt function_name program_unit = 
	if function_name = "" then stmt global_scope "" program_unit else stmt (try Hashtbl.find function_map function_name with Not_found -> raise(Failure(function_name ^ " not in function_map for statements"))) function_name program_unit

  in
 
  let get_function_type function_name function_line = match function_line with 
       Ast.VarUnit(s) -> Sast.VarUnit(check_vdecl_function function_name s)
     | Ast.StmtUnit(st) -> Sast.StmtUnit(check_stmt function_name st)
     | _ -> raise (Failure("this declaration is not defined"))
  in
  
  (*let add_formals formal fname = match formal with 
        Ast.Decl(t, n) -> let f_map = (try Hashtbl.find function_map fname with Not_found -> raise(Failure("function not in function_map for adding formals"))) in Hashtbl.add f_map n t 
      | Ast.ArrDecl(t, n, el) -> let f_map = (try Hashtbl.find function_map fname with Not_found -> raise(Failure("function not in function_map for adding formals"))) in List.iter (fun a -> ignore(check_expr_legal a f_map)) el; Hashtbl.add f_map n t*)
  
  let add_hash x = match x with 
	Ast.Decl(t, n) -> (t, n)
      | Ast.ArrDecl(t, n, _) -> (t, n)
  in 
  
  let update_maps fd =  
	ignore(Hashtbl.add function_map_type fd.fname fd.typ);
	(*ignore(Hashtbl.add function_map_formals fd.fname fd.formals;*)
	let map = List.map (fun x -> add_hash x) fd.formals in Hashtbl.add function_map_formals fd.fname map;  
	ignore(Hashtbl.add function_map fd.fname (Hashtbl.create 20));
        (*List.iter (fun n -> ignore(add_formals n fd.fname)) fd.formals;*)
	Hashtbl.add function_map_length fd.fname (List.length fd.formals) 
  in
  
  let extract_name formal = match formal with 
        Ast.Decl(_, n) -> n 
      | Ast.ArrDecl(_, n, _) -> n
  in 

  let rec resolve_body name body = match body with 
	 [] -> []
       | head::tail -> let r = get_function_type name head in r::(resolve_body name tail)
  in 

  let check_not_void_general formal = match formal with 
       Ast.Decl(t, _) -> check_not_void t
     | Ast.ArrDecl(t, _, _) -> check_not_void t

  in 

  let check_fdecl fd = 
	ignore(if Hashtbl.mem function_map fd.fname = false then update_maps fd else raise(Failure("duplicate function found"))); 

	report_duplicate (fun _ -> "duplicate formal") (List.map (fun x -> extract_name x) fd.formals); 
	List.iter (fun n -> check_not_void_general n) fd.formals;
        (*resolve all formals, resolve function body, return a Sast type of FuncUnit with new values*)
	let func_formals = List.map (fun a -> check_vdecl_function fd.fname a) fd.formals and
        new_body = resolve_body fd.fname fd.body in let new_fdecl = {s_typ=convert_type fd.typ; s_fname=fd.fname; s_formals=func_formals; s_body = new_body} in Sast.FuncUnit(new_fdecl) 
  
  in

  let get_type = function
        Ast.VarUnit(s)  -> Sast.VarUnit(check_vdecl s)
      | Ast.FuncUnit(fd) -> check_fdecl fd
      | Ast.StmtUnit(st) -> Sast.StmtUnit(check_stmt "" st)
  in
 
  List.map get_type program_list;
  (*let rec check_types program_list = match program_list with 
        [] -> []
      | head::tail -> let r = get_type head in r::(check_types tail)
  in 
 
  check_types program_list *)

