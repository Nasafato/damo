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
let convert (program_list) =
  (*map for function name and list of variables, including formals and locals*)
  let function_map = StringMap.empty in 
  (*maps for function name and type info, such as return type and length of paramter list-type checking of actuals done at runtime*)
  let function_map_type = StringMap.empty in 
  let function_map_length = StringMap.empty in 
  let function_map_formals = StringMap.empty in 

  let extract_type s_expr = match s_expr with 
	Sast.Id(t, _)             -> t
      | Sast.Noexpr(t)            -> t 
      | Sast.Binop(t, _, _ ,_)    -> t
      | Sast.IntLit(t, _)         -> t
      | Sast.BoolLit(t, _)        -> t
      | Sast.StringLit(t, _)      -> t
      | Sast.NumLit(t, _)         -> t
      | Sast.ArrID(t, _, _)       -> t
      | Sast.Unop(t, _, _)        -> t
      | Sast.Assign(t, _, _)      -> t
      | Sast.Call(t, _, _)        -> t
      | Sast.Indexing(t, _, _)    -> t 
  in

  let check_assign lvaluet rvaluet err = if lvaluet == rvaluet then lvaluet else raise err in 
  (* global scope record for all global variables *) 
  let global_scope = StringMap.empty in
  
  let type_of_identifier s env =
      try StringMap.find s env
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in


  let printing_functions = ["print" ; "print_int" ; "print_num" ; "print_bool"] in
  let symbol_functions = ["left" ; "right" ; "operation" ; "value" ] in
  let built_in_functions = printing_functions @ symbol_functions in
  let rec expr env e = match e with 
        Ast.IntLit(i)    -> Sast.IntLit(Sast.Int, i)
      | Ast.BoolLit(b)   -> Sast.BoolLit(Sast.Bool, b)
      (* NEW an expression can be a string literal *)
      | Ast.NumLit(n)    -> Sast.NumLit(Sast.Num, n)
      | Ast.StringLit(s) -> Sast.StringLit(Sast.String, s)
      | Ast.Id(s) -> Sast.Id(type_of_identifier s env, s)
      | Ast.Binop(e1, op, e2) as e -> let e1' = expr env e1 and e2' = expr env e2 in let t1 = extract_type e1' and t2 = extract_type e2' in
      	(match op with
              Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Int && t2 = Sast.Num )-> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Num && t2 = Sast.Int) -> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Int && t2 = Sast.Int -> Sast.Binop(Sast.Int, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Num && t2 = Sast.Num -> Sast.Binop(Sast.Num, e1', op, e2')
            (* NEW allow symbols to be operated on with nums *)
            (*| Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Ast.Symbol && t2 = Ast.Num -> Sast.Binop(Sast.Symbol, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Ast.Num && t2 = Ast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')*)
            (* NEW only allow for comparison of ints and nums *)
          	| Ast.Equal | Ast.Neq when t1 = t2 && (t1 = Sast.Int || t1 = Sast.Num) -> Sast.Binop(Sast.Bool, e1', op, e2')
          	| Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq when t1 = Sast.Int && t2 = Sast.Int -> Sast.Binop(Sast.Bool, e1', op, e2')
          	| Ast.And | Ast.Or when t1 = Sast.Bool && t2 = Sast.Bool -> Sast.Binop(Sast.Bool, e1', op, e2')
                  | _ -> raise (Failure ("illegal binary operator "))
        )
      | Ast.Unop(op, e) as ex -> let e' = expr env e in let t = extract_type e' in
        (match op with
           Ast.Neg when t = Sast.Int -> Sast.Unop(Sast.Int, op, e')
         | Ast.Not when t = Sast.Bool -> Sast.Unop(Sast.Bool, op, e')
         | _ -> raise (Failure ("illegal unary operator ")))
      | Ast.Noexpr -> Sast.Noexpr(Sast.Void)
      | Ast.Assign(var, e) as ex -> let lt = type_of_identifier var env
        and rt = extract_type (expr env e) in
        check_assign lt rt (Failure ("illegal assignment "))
      | Ast.Call(fname, actuals) as call -> if List.length actuals != StringMap.find fname function_map_length then
          raise (Failure ("incorrect number of arguments"))
        else
          List.iter2 (fun (ft, _) e -> let et = extract_type (expr global_scope e) in
                       ignore (check_assign ft et
                                 (Failure ("illegal actual argument found, wrong type"))))
            (StringMap.find fname function_map_formals) actuals;
        Sast.Call((StringMap.find fname function_map_type), fname, actuals)
    in
  let report_duplicate_map var env = 
    if StringMap.mem var env == false then true else raise(Failure("duplicate found")); 
  
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
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  
  let check_not_void_map argOne argTwo = match argTwo with 
      Void -> raise(Failure("cannot have void variable type"))
    | _ -> ()
  in
 
  let check_expr_legal t e env = let e' = expr env e in if extract_type e' == t then e' else raise(Failure ("Incorrect type")) in 

  let check_vdecl = function 
      Ast.Decl(t,name) -> ignore(StringMap.add name t global_scope); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; ignore(report_duplicate_map name global_scope); Sast.Decl(t, name)
    | Ast.InitDecl(t, n, e) -> ignore(StringMap.add n t global_scope); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; ignore(report_duplicate_map n global_scope); let e' = expr global_scope e in if e' == t then Sast.InitDecl(t,n,e') else raise(Failure ("incorrect type"))
    | Ast.ArrDecl(t, n, l) -> ignore(StringMap.add n t global_scope); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; ignore(report_duplicate_map n global_scope); let l' = List.map (fun a -> check_expr_legal t a env) l in Sast.ArrDecl(t, n, l')    
        
  in
  
  let check_vdecl_function function_name function_line = match function_line with  
 	Ast.Decl(t,name) -> let f_map = StringMap.find function_name function_map in ignore(StringMap.add name t f_map); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; ignore(report_duplicate_map name f_map); Sast.Decl(t, name)
    | Ast.InitDecl(t, n, e) -> let f_map = StringMap.find function_name function_map in ignore(StringMap.add n t f_map); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; ignore(report_duplicate_map n f_map); let e' = expr e f_map in if e' == t then Sast.InitDecl(t,n,e') else raise(Failure ("incorrect type"))
    | Ast.ArrDecl(t, n, l) -> let f_map = StringMap.find function_name function_map in ignore(StringMap.add n t f_map); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; ignore(report_duplicate_map n f_map); let l' = List.map (fun a -> check_expr_legal t a f_map) l in Sast.ArrDecl(t, n, l') 
		
  in
  
  let check_bool_expr env e = if extract_type (expr env e) != Bool
      then raise (Failure ("expected boolean expression "))
      else () in

    (* verify a statement or throw an exception *)
  let rec stmt env fname s = match s with 
      | Ast.Expr(e) -> Sast.Expr(expr env e)
      | Ast.Return(e) -> if (String.compare fname "") != 0 then raise(Failure("can't have return type outside of function")) else let t = extract_type (expr env e) in if t = StringMap.find fname function_map_type then () else raise (Failure ("incorrect return type"))
      | Ast.If(p, b1, b2) -> check_bool_expr p; Sast.If(expr env p, stmt env b1, stmt env b2)
      | Ast.For(e1, e2, e3, st) -> check_bool_expr e2; Sast.For(expr env e1, expr env e2, expr env e3, stmt env st)
      | Ast.While(p, s) -> check_bool_expr p; Sast.While(expr env p, stmt env s)
   
  in
  let check_stmt function_name program_unit = 
	if (String.compare function_name "") != 0 then stmt global_scope "" program_unit else stmt (StringMap.find function_name function_map) function_name program_unit

  in
 
  let get_function_type function_name function_line = match function_line with 
       Ast.VarFunit -> check_vdecl_function function_name function_line
     | Ast.StmtFunit -> check_stmt function_name function_line 
  in
  
  let update_maps fd = 
	StringMap.add fd.name fd.typ function_map_type;
	StringMap.add fd.name fd.formals function_map_formals;
	StringMap.add fd.name StringMap.empty function_map;
        List.iter (fun n -> StringMap.add (snd n) (fst n) (StringMap.find fd.name function_map)) fd.formals;
	StringMap.add fd.name (List.length fd.formals) function_map_length
  in

  let rec resolve_body name body = match body with 
	 [] -> []
       | head::tail -> let r = get_function_type name head in r::(resolve_body name tail)
  in 

  let check_fdecl fd = 
	ignore(if StringMap.mem fd.name function_map == false then update_maps fd else raise(Failure("duplicate found"))); 

	report_duplicate (fun n -> "duplicate formal") (List.map snd fd.formals); 
	List.iter (check_not_void (fun n -> "can't have void variable")) (List.map snd fd.formals);
        (*resolve all formals, resolve function body, return a Sast type of FuncUnit with new values*)
	let func_formals = List.map (fun a -> expr (StringMap.find fd.name function_map) a) fd.formals and
        new_body = resolve_body fd.name fd.body 
	in Sast.FuncUnit(fd.typ, fd.fname, func_formals, new_body) 
  in

  let get_type program_unit = function
        Ast.VarUnit  -> check_vdecl program_unit
      | Ast.FuncUnit -> check_fdecl program_unit
      | Ast.StmtUnit -> check_stmt program_unit
  in
 
  let rec check_types program_list = match program_list with 
        [] -> []
      | head::tail -> let r = get_type head in r::(check_types tail)
  in 
 
  check_types program_list 
   

