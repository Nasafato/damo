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
  in
 
  let extract_type_lvalue lvalue = match lvalue with 
        Sast.Idl(t, _)            -> t
      | Sast.ArrIdl(t, _, _)      -> t
  in 

  let check_assign lvaluet rvaluet err = if lvaluet == rvaluet then lvaluet else raise err in 
  (* global scope record for all global variables *) 
  let global_scope = StringMap.empty in
  
  let type_of_identifier s env =
      try StringMap.find s env
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  
  let find_func fname =  
      try StringMap.find fname function_map 
      with Not_found -> raise (Failure ("undeclared function"))
  in 

  let printing_functions = ["print" ; "print_int" ; "print_num" ; "print_bool"] in
  let symbol_functions = ["left" ; "right" ; "operation" ; "value" ] in
  let built_in_functions = printing_functions @ symbol_functions in
  let check_index_type index = match index with 
        Sast.Int -> ()
      | _ -> raise (Failure ("must have int type as index"))
  in 
 
  let rec check_expr_legal e env = let e' = expr env e in if extract_type e' == Sast.Int then e' else raise(Failure ("Incorrect type")) 
  
  
  and check_lvalue env lvalue  = match lvalue with  
        Ast.Idl(s) -> Sast.Idl(convert_type (type_of_identifier s env), s)
      | Ast.ArrIdl(s, el) -> List.iter (fun a -> ignore(check_expr_legal a env)) el; let el' = List.map (fun a -> expr env a) el in Sast.ArrIdl(convert_type (type_of_identifier s env), s, el')
 
  and expr env e = match e with 
        Ast.IntLit(i)    -> Sast.IntLit(Sast.Int, i)
      | Ast.BoolLit(b)   -> Sast.BoolLit(Sast.Bool, b)
      (* NEW an expression can be a string literal *)
      | Ast.NumLit(n)    -> Sast.NumLit(Sast.Num, n)
      | Ast.StringLit(s) -> Sast.StringLit(Sast.String, s)
      | Ast.Id(s) -> Sast.Id(convert_type (type_of_identifier s env), s)
      | Ast.ArrId(a, el) -> List.iter (fun x -> ignore(check_expr_legal x env)) el; let el' = List.map(fun x -> expr env x) el in  Sast.ArrId(convert_type (type_of_identifier a env), a, el')
      | Ast.Binop(e1, op, e2) as e -> let e1' = expr env e1 and e2' = expr env e2 in let t1 = extract_type e1' and t2 = extract_type e2' in
      	(match op with
              Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Int && t2 = Sast.Num )-> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Sast.Num && t2 = Sast.Int) -> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Int && t2 = Sast.Int -> Sast.Binop(Sast.Int, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Sast.Num && t2 = Sast.Num -> Sast.Binop(Sast.Num, e1', op, e2')
            (* NEW allow symbols to be operated on with nums *)
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Sast.Symbol && t2 = Sast.Num -> Sast.Binop(Sast.Symbol, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Sast.Num && t2 = Sast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')
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
      | Ast.Assign(lvalue, e) -> let new_lvalue = check_lvalue env lvalue in let lt = extract_type_lvalue new_lvalue and rt = extract_type (expr env e) in ignore(check_assign lt rt (Failure ("illegal assignment "))); Sast.Assign(new_lvalue, expr env e)
      | Ast.Call(fname, actuals) -> ignore(find_func fname); if List.length actuals != StringMap.find fname function_map_length then
          raise (Failure ("incorrect number of arguments"))
        else 
	  let new_actuals = List.map (fun e -> expr env e) actuals in
          List.iter2 (fun (ft, _) e -> let et = extract_type e in
                       ignore (check_assign ft et
                                 (Failure ("illegal actual argument found, wrong type"))))
            (StringMap.find fname function_map_formals) new_actuals;
        Sast.Call(convert_type (StringMap.find fname function_map_type), fname, new_actuals)
    
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
  let check_not_void t = match t with 
      Ast.Void -> raise (Failure ("can't have void variable"))  
    | _ -> ()
  in


  let check_not_void_map argOne argTwo = match argTwo with 
      Ast.Void -> raise(Failure("cannot have void variable type"))
    | _ -> ()
  in
 
    let check_vdecl = function 
      Ast.Decl(t,name) ->  ignore(report_duplicate_map name global_scope); ignore(StringMap.add name t global_scope); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; Sast.Decl(convert_type t, name)
    | Ast.ArrDecl(t, n, l) -> ignore(report_duplicate_map n global_scope); ignore(StringMap.add n t global_scope); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) global_scope; List.iter (fun a -> ignore(check_expr_legal a global_scope)) l; let l' = List.map (fun a -> expr global_scope a ) l in Sast.ArrDecl(convert_type t, n, l')    
        
  in
  
  let check_vdecl_function function_name function_line = match function_line with  
 	Ast.Decl(t,name) -> let f_map = StringMap.find function_name function_map in ignore(report_duplicate_map name f_map); ignore(report_duplicate_map name global_scope); ignore(StringMap.add name t f_map); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; Sast.Decl(convert_type t, name)
    | Ast.ArrDecl(t, n, l) -> let f_map = StringMap.find function_name function_map in ignore(report_duplicate_map n f_map); ignore(report_duplicate_map n global_scope); ignore(StringMap.add n t f_map); StringMap.iter (fun a1 a2 -> check_not_void_map a1 a2) f_map; List.iter (fun a -> ignore(check_expr_legal a f_map)) l; let l' = List.map (fun a -> expr f_map a) l in Sast.ArrDecl(convert_type t, n, l') 
		
  in
  
  let check_bool_expr env e = if extract_type (expr env e) != Sast.Bool
      then raise (Failure ("expected boolean expression "))
      else () in

    (* verify a statement or throw an exception *)
  let rec stmt env fname s = match s with 
        Ast.Expr(e) -> Sast.Expr(expr env e)
      | Ast.Block(sl) -> let sl' = List.map (fun a -> stmt env fname a) sl in Sast.Block(sl')
      | Ast.Return(e) -> if fname <> "" then raise(Failure("can't have return type outside of function")) else let t = extract_type (expr env e) in if t = convert_type (StringMap.find fname function_map_type) then Sast.Return(expr env e) else raise (Failure ("incorrect return type"))
      | Ast.If(p, b1, b2) -> check_bool_expr env p; Sast.If(expr env p, stmt env fname b1, stmt env fname b2)
      | Ast.For(e1, e2, e3, st) -> check_bool_expr env e2; Sast.For(expr env e1, expr env e2, expr env e3, stmt env fname st)
      | Ast.While(p, s) -> check_bool_expr env p; Sast.While(expr env p, stmt env fname s)
   
  in
  let check_stmt function_name program_unit = 
	if function_name <> "" then stmt global_scope "" program_unit else stmt (StringMap.find function_name function_map) function_name program_unit

  in
 
  let get_function_type function_name function_line = match function_line with 
       Ast.VarFunit(s) -> Sast.VarFunit(check_vdecl_function function_name s)
     | Ast.StmtFunit(st) -> Sast.StmtFunit(check_stmt function_name st)
  in
  
  let add_formals formal fname = match formal with 
        Ast.Decl(t, n) -> let f_map = StringMap.find fname function_map in StringMap.add n t f_map
      | Ast.ArrDecl(t, n, el) -> let f_map = StringMap.find fname function_map in List.iter (fun a -> ignore(check_expr_legal a f_map)) el; StringMap.add n t f_map
  in
 
  let update_maps fd =  
	ignore(StringMap.add fd.fname fd.typ function_map_type);
	ignore(StringMap.add fd.fname fd.formals function_map_formals);
	ignore(StringMap.add fd.fname StringMap.empty function_map);
        (*List.iter (fun n -> ignore(add_formals n fd.fname)) fd.formals;*)
	StringMap.add fd.fname (List.length fd.formals) function_map_length
  in
  
  let extract_name formal = match formal with 
        Ast.Decl(t, n) -> n 
      | Ast.ArrDecl(t, n, el) -> n
  in 

  let rec resolve_body name body = match body with 
	 [] -> []
       | head::tail -> let r = get_function_type name head in r::(resolve_body name tail)
  in 

  let check_not_void_general formal = match formal with 
       Ast.Decl(t, n) -> check_not_void t
     | Ast.ArrDecl(t, n, el) -> check_not_void t

  in 

  let check_fdecl fd = 
	ignore(if StringMap.mem fd.fname function_map = false then update_maps fd else raise(Failure("duplicate found"))); 

	report_duplicate (fun n -> "duplicate formal") (List.map (fun x -> extract_name x) fd.formals); 
	List.iter (fun n -> check_not_void_general n) fd.formals;
        (*resolve all formals, resolve function body, return a Sast type of FuncUnit with new values*)
	let func_formals = List.map (fun a -> check_vdecl_function fd.fname a) fd.formals and
        new_body = resolve_body fd.fname fd.body in let new_fdecl = {s_typ=(convert_type fd.typ),s_fname=fd.fname, s_formals=func_formals, s_body = new_body} in Sast.FuncUnit(new_fdecl) 
  
  in

  let get_type program_unit = function
        Ast.VarUnit(s)  -> Sast.VarUnit(check_vdecl s)
      | Ast.FuncUnit(fd) -> check_fdecl fd
      | Ast.StmtUnit(st) -> Sast.StmtUnit(check_stmt "" st)
  in
 
  let rec check_types program_list = match program_list with 
        [] -> []
      | head::tail -> let r = get_type head in r::(check_types tail)
  in 
 
  check_types program_list 
   

