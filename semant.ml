(* Semantic checking for the damo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)
type env_var = {v_type: Sast.t; v_expr: Sast.expr; v_name: string}
type env_global = {sym_table: env_var StringMap.t;}
type env_function = {env_name: string; sym_table: env_var StringMap.t; env_ret_type: Sast.t}

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
      | Sast.Binop(_, t, _ ,_)    -> t
      | Sast.IntLit(t, _)         -> t
      | Sast.BoolLit(t, _)        -> t
      | Sast.StringLit(t, _)      -> t
      | Sast.NumLit(t, _)         -> t
      | Sast.ArrID(t, _, _)       -> t
      | Sast.Unop(t, _, _)        -> t
      | Sast.Assign(t, _, _)      -> t
      | Sast.Call(t, _, t, _)     -> t
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
  let built_in_functions = reserved @ printing_functions @ symbol_functions in
  let rec expr env= function
	Ast.IntLit(i) _ -> Sast.IntLit(Sast.Int, i)
      | Ast.BoolLit(b) _ -> Sast.BoolLit(Sast.Bool, b)
      (* NEW an expression can be a string literal *)
      | Ast.NumLit(n) _ -> Sast.NumLit(Sast.Num, n)
      | Ast.StringLit(s) _ -> Sast.StringLit(Sast.String, s)
      | Ast.Id(s) -> Sast.Id(type_of_identifier s env, s)
      | Ast.Binop(e1, op, e2) as e -> let e1' = expr env e1 and e2' = expr env e2 in let t1 = extract_type e1' and t2 = extract_type e2' in
      	(match op with
              Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Ast.Int && t2 = Ast.Num )-> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when (t1 = Ast.Num && t2 = Ast.Int) -> Sast.Binop(Sast.Num, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Ast.Int && t2 = Ast.Int -> Sast.Binop(Sast.Int, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod | Ast.Exp | Ast.Log when t1 = Ast.Num && t2 = Ast.Num -> Sast.Binop(Sast.Num, e1', op, e2')
            (* NEW allow symbols to be operated on with nums *)
            (*| Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Ast.Symbol && t2 = Ast.Num -> Sast.Binop(Sast.Symbol, e1', op, e2')
            | Ast.Add | Ast.Sub | Ast.Mult | Ast.Div  when t1 = Ast.Num && t2 = Ast.Symbol -> Sast.Binop(Sast.Symbol, e1', op, e2')*)
            (* NEW only allow for comparison of ints and nums *)
          	| Ast.Equal | Ast.Neq when t1 = t2 && (t1 = Ast.Int || t1 = Ast.Num) -> Sast.Binop(Sast.Bool, e1', op, e2')
          	| Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq when t1 = Ast.Int && t2 = Ast.Int -> Sast.Binop(Sast.Bool, e1', op, e2')
          	| Ast.And | Ast.Or when t1 = Ast.Bool && t2 = Ast.Bool -> Sast.Binop(Sast.Bool, e1', op, e2')
                  | _ -> raise (Failure ("illegal binary operator "))
        )
      | Ast.Unop(op, e) as ex -> let e' = expr env e in let t = extract_type e' in
        (match op with
           Ast.Neg when t = Ast.Int -> Sast.Unop(Sast.Int, op, e')
         | Ast.Not when t = Ast.Bool -> Sast.Unop(Sast.Bool, op, e')
         | _ -> raise (Failure ("illegal unary operator ")))
      | Ast.Noexpr -> Sast.Void
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
  
  let check_expr_legal t e env = let e' = expr env e in if e' != t then e' else raise(Failure ("Incorrect type")) in 

  let check_vdecl program_unit = function 
      Ast.Decl(t,name) -> ignore(StringMap.add name t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map name global_scope); Sast.Decl(t, name)
    | Ast.InitDecl(t, n, e) -> ignore(StringMap.add n t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map n global_scope); let e' = expr global_scope e in if e' == t then Sast.InitDecl(t,n,e') else raise(Failure ("incorrect type"))
    | Ast.ArrDecl(t, n, l) -> ignore(StringMap.add n t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map n global_scope); let l' = List.map (fun a -> check_expr_legal t a env) l in Sast.ArrDecl(t, n, l')    
        
  in
  
  let check_vdecl_function function_line = 
 	Ast.Decl(t,name) -> ignore(StringMap.add name t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map name global_scope); Sast.Decl(t, name)
    | Ast.InitDecl(t, n, e) -> ignore(StringMap.add n t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map n global_scope); let e' = expr e global_scope in if e' == t then Sast.InitDecl(t,n,e') else raise(Failure ("incorrect type"))
    | Ast.ArrDecl(t, n, l) -> ignore(StringMap.add n t global_scope); List.iter (check_not_void (fun n -> "illegal void ")) global_scope; ignore(report_duplicate_map n global_scope); let l' = List.map (fun a -> check_expr_legal t a global_scope) l in Sast.ArrDecl(t, n, l') 
		
  in
 
  let get_function_type fd.name function_line = function
       Ast.VarFunit -> check_vdecl_function function_line
     | Ast.StmtFunit -> check_stmt_function function_ line 
  in
  
  let update_maps fd = 
	StringMap.add fd.name fd.typ function_map_type;
	StringMap.add fd.name fd.formals function_map_formals;
	StringMap.add fd.name StringMap.empty function_map;
        List.iter (fun n -> StringMap.add (snd n) (fst n) (StringMap.find fd.name function_map)) fd.formals;
	StringMap.add fd.name (List.length fd.formals) function_map_length
  in

  let check_fdecl fd = 
	if StringMap.mem fd.name function_map == false then update_maps fd else raise(Failure("duplicate found")); 

	report_duplicate (fun n -> "duplicate formal") (List.map snd fd.formals); 
	List.iter (check_not_void (fun n -> "can't have void variable")) (List.map snd fd.formals);
        (*resolve all formals, resolve function body, return a Sast type of FuncUnit with new values*)
	let resolve_fds = List.map (fun a -> expr (StringMap.find fd.name function_map) a) fd.formals in
        let new_body = get_function_type fd.name fd.body in  
  in

  let get_type program_unit = function
        Ast.VarUnit  -> check_vdecl program_unit
      | Ast.FuncUnit -> check_fdecl program_unit
      | Ast.StmtUnit -> check_stmt program_unit 
  in
 
  let rec check_types program_list = function
        [] -> []
      | head::tail -> let r = get_type head in r::check_types tail
  in 
  
   
  let convert_type t = match t with 
        Ast.Int     -> Sast.Int
      | Ast.Num     -> Sast.Num
      | Ast.String  -> Sast.String
      | Ast.Bool    -> Sast.Bool
      | Ast.Void    -> Sast.Void
  in
  
  (**** Checking Functions ****)

     
  (* NEW check for prior existence of built in functions *)
  ignore (List.map (fun name -> if List.mem name function_list
    then raise (Failure ("function " ^ name ^ " may not be defined")) else ()) built_in_functions);

  report_duplicate (fun n -> "duplicate function " ^ n) function_list;

  (* Function declaration for named functions *)
  let built_in_decls = StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(String, "x")];
      locals = []; body = [] } (StringMap.add "print_num"
     { typ = Void; fname = "print_num"; formals = [(Num, "x")];
       locals = []; body = [] } (StringMap.add "print_int"
     { typ = Void; fname = "print_int"; formals = [(Int, "x")];
       locals = []; body = [] } (StringMap.add "print_bool"
     { typ = Void; fname = "print_bool"; formals = [(Bool, "x")];
       locals = []; body = [] } (StringMap.singleton "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] }))))
   in

  (* NEW additionally, add functions related to symbols *)
  let built_in_decls = StringMap.add "value"
     { typ = Void; fname = "value"; formals = [(Symbol, "x")];
      locals = []; body = [] } (StringMap.add "operation"
     { typ = Void; fname = "operation"; formals = [(Symbol, "x")];
      locals = []; body = [] } (StringMap.add "right"
     { typ = Void; fname = "right"; formals = [(Symbol, "x")];
      locals = []; body = [] } (StringMap.add "left"
     { typ = Void; fname = "left"; formals = [(Symbol, "x")];
      locals = []; body = [] } built_in_decls)))
   in

  (* Create a hash map of functions, where key is function name *)
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
      built_in_decls functions
  in

  (* Determine whether function is contained in hash map function_decls *)
  let function_decl s = try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
                                        " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
                                        " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);
    
    let func_scope = {env_name = func.fname; sym_table = StringMap.empty; env_ret_type = func.typ} in
    (* Type of each variable (global, formal, or local *)
    let local_vars = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals)
    in
        
 
    let check_bool_expr e = if expr e != Bool
      then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
        Block sl -> let rec check_block = function
          (* NEW this is subtle, but I think s should be verified using expr, not stmt
             see AST, where stmt can take on value Return of type expr *)
            [Return _ as s] -> stmt s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> check_block (sl @ ss)
          | s :: ss -> stmt s ; check_block ss
          | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
          raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                          string_of_typ func.typ ^ " in " ^ string_of_expr e))

      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
        ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
      | Initialize(t, n, e) -> ignore(StringMap.add n t symbols); ignore(check_assign t (expr e) (Failure ("illegal assignment")))
    in
    stmt (Block func.body)

  in
  List.iter check_function functions
