(* Semantic checking for the damo compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (topstmts, functions) =
  let globals = [] in

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

  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet == rvaluet then lvaluet
    else raise err
  in

  (**** Checking Global Variables ****)

  (*
    We need to implement scoping now, since toplevel variable assignments
    and declarations can occur in an arbitrary order. It's up to semantic
    checking to see whether variable reads and function calls are allowed
    in scope.
   *)

  (*
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);
  *)

  (**** Checking Functions ****)

  (* NEW list of all built in functions *)
  let reserved = ["main"] in
  let printing_functions = ["print" ; "print_int" ; "print_num" ; "print_bool"] in
  let symbol_functions = ["left" ; "right" ; "operation" ; "value" ] in
  let built_in_functions = reserved @ printing_functions @ symbol_functions in
  
  (* NEW create list of function names *)
  let function_list = List.map (fun fd -> fd.fname) functions in

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

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals)
    in
        
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Return the type of an expression or throw an exception *)
    let rec expr = function

	IntLit _ -> Int
      | BoolLit _ -> Bool
      (* NEW an expression can be a string literal *)
      | NumLit _ -> Num
      | StringLit _ -> String
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in

      	(match op with
              Add | Sub | Mult | Div | Mod | Exp | Log when (t1 = Int && t2 = Num )-> Num
            | Add | Sub | Mult | Div | Mod | Exp | Log when (t1 = Num && t2 = Int) ->  Num
            | Add | Sub | Mult | Div | Mod | Exp | Log when t1 = Int && t2 = Int -> Int
            | Add | Sub | Mult | Div | Mod | Exp | Log when t1 = Num && t2 = Num -> Num
            (* NEW allow symbols to be operated on with nums *)
            | Add | Sub | Mult | Div  when t1 = Symbol && t2 = Num -> Symbol
            | Add | Sub | Mult | Div  when t1 = Num && t2 = Symbol -> Symbol
            (* NEW only allow for comparison of ints and nums *)
          	| Equal | Neq when t1 = t2 && (t1 = Int || t1 = Num) -> Bool
          	| Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
          	| And | Or when t1 = Bool && t2 = Bool -> Bool
                  | _ -> raise (Failure ("illegal binary operator " ^
                        string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_expr e))

        )
      | Unop(op, e) as ex -> let t = expr e in
        (match op with
           Neg when t = Int -> Int
         | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
                                string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
        and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
                                     " = " ^ string_of_typ rt ^ " in " ^
                                     string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
        if List.length actuals != List.length fd.formals then
          raise (Failure ("expecting " ^ string_of_int
                            (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
        else
          List.iter2 (fun (ft, _) e -> let et = expr e in
                       ignore (check_assign ft et
                                 (Failure ("illegal actual argument found " ^ string_of_typ et ^
                                           " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
            fd.formals actuals;
        fd.typ
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
