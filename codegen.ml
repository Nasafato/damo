(* Code generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)
exception NotImplemented
exception IllegalType


let translate (topstmts, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "damo"
  and i32_t  = L.i32_type  context
  and num_t = L.double_type context
  and i8_t   = L.i8_type context
  and str_t = L.pointer_type (L.i8_type context)
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context 
  and symbol_t = L.pointer_type (L.i8_type context) in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Num -> num_t
    | A.String -> str_t
    | A.Symbol -> symbol_t
    | A.Void -> void_t in


  (* Declare each global variable; remember its value in a map *)

  let global_vars = StringMap.empty in

  let filtered_main_binds = 
    topstmts
    |> List.filter (function
      | A.Bind _ -> true
      | _ -> false
    ) in
  let main_binds = 
    filtered_main_binds
    |> List.map (function | A.Bind b -> b) in

  let filtered_main_stmts =
    topstmts
    |> List.filter (function
      | A.Bind _ -> false
      | _ -> true
    ) 
    |> List.rev
    in
  
  let main_function = {
    A.typ = A.Int;
    A.fname = "main";
    A.formals = [];
    A.locals = main_binds;
    A.body = filtered_main_stmts;
  } in

(*
  let _ = Printf.printf "%s" (String.concat "\n" (List.map A.string_of_stmt main_function.A.body)) in
  *)

  let functions = main_function :: functions in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in


  let pwr_fxn_num = Llvm.declare_function "pow" ( Llvm.function_type num_t [| num_t; num_t |] ) the_module in
  let log_fxn_num = Llvm.declare_function "log" ( Llvm.function_type num_t [|  num_t |] ) the_module in
  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* NEW formatting string for using printf on strings *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmtint" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmtstr" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "floatstr" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n (local, t) m in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n (local_var, t) m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in
    let init_hash = Hashtbl.create 20 in 
    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> try StringMap.find n global_vars
      with Not_found -> try Hashtbl.find init_hash n
      with Not_found -> raise (Failure ("not defined yet")) 
    in
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.Literal i -> L.const_int i32_t i 
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      (* NEW for creating strings from expressions *)
      | A.StringLit st -> L.build_global_stringptr st "tmp" builder 
      | A.NumLit num -> L.const_float num_t num
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> let (s_v,_) = lookup s in L.build_load s_v s builder
      | A.Binop (e1, op, e2) ->

        let e1' = expr builder e1
        and e2' = expr builder e2 in

        let bop_int_with_num (e1, e2) =
          (match e1, e2 with
           | A.Literal i , A.NumLit _ -> expr builder (A.NumLit (float_of_int (i))), e2', A.Num
           | A.NumLit _, A.Literal i -> e1', expr builder (A.NumLit (float_of_int (i)) ), A.Num
           | A.Id(n), A.Id(i) -> let _, t1 = lookup n in 
             let _, t2 = lookup i in
             if (t1 = A.Num) && (t2 = A.Int) then 
               e1', (L.build_sitofp e2' num_t "cast" builder), A.Num
             else if (t1 = A.Int) && (t2 = A.Num) then 
               (L.build_sitofp e1' num_t "cast" builder), e2', A.Num
             else if t1 = A.Int && t2 = A.Int then
              e1', e2', A.Int
             else if t1 = A.Num && t2 = A.Num then
               e1', e2', A.Num
             else if t1 = A.Bool && t2 = A.Bool then
               e1', e2', A.Bool
             else
               raise( IllegalType ) (* there should be no other types coming this way*)
           | A.Id(n), A.Literal(_) -> let _, t1 = lookup n in 
              if t1 = A.Num then 
                e1', (L.build_sitofp e2' num_t "cast" builder), A.Num
              else
                e1', e2', A.Int
           | A.Literal(_), A.Id(n) -> let _, t1 = lookup n in 
              if t1 = A.Num then 
                (L.build_sitofp e1' num_t "cast" builder), e2', A.Num
              else
                e1', e2', A.Int
            | A.Id(i), A.NumLit(_) -> let _, t1 = lookup i in 
              if t1 = A.Int then 
                (L.build_sitofp e1' num_t "cast" builder), e2', A.Num
              else
                e1', e2', A.Num
            | A.NumLit(_), A.Id(n) -> let _, t1 = lookup n in 
              if t1 = A.Int then 
                e1', (L.build_sitofp e2' num_t "cast" builder), A.Num
              else
                e1', e2', A.Num
            | A.BoolLit(_), A.BoolLit(_) -> e1', e2', A.Bool
            | A.Literal(_), A.Literal(_) -> e1', e2', A.Int
            | A.NumLit(_), A.NumLit(_) -> e1', e2', A.Num
            | _, _ -> raise( IllegalType )
          ) in
        (* if we have int+num, cast int into a float and continue*)
        let (e1', e2', expr_type) = bop_int_with_num (e1, e2) in 

        (* Exp and Log are run time functions because they are without llvm equivalents *)
        if op = A.Exp then 
        (* For exp functions, if num just call the exp function, else cast int to num
        then build exp funtion then cast back to int *)
            let exp_types = (match expr_type with
            A.Num -> Llvm.build_call pwr_fxn_num [| e1'; e2' |] "pow_func" builder
            | A.Int -> let e1_cast = L.build_sitofp e1' num_t "cast" builder in
               let e2_cast = L.build_sitofp e2' num_t "cast" builder in
               let pow_cast =  Llvm.build_call pwr_fxn_num [| e1_cast; e2_cast |] "pow_func" builder in
               L.build_fptosi pow_cast i32_t "cast" builder
            | _ -> raise( NotImplemented )
            ) in exp_types
        else if op = A.Log then
        (* For log functions, if num, then get log_e of the num, then log_e of the base
        then divide them using llvm divide fxn
        for int, first convert, then do the above, then convert back *)
         let log_types = (match expr_type with
              A.Num -> let top_log = Llvm.build_call log_fxn_num [| e2' |] "log_func" builder in
                       let bottom_log =  Llvm.build_call log_fxn_num [| e1' |] "log_func" builder in
                       Llvm.build_fdiv top_log bottom_log "tmp" builder
              | A.Int -> let e1_cast = L.build_sitofp e1' num_t "cast" builder in
                       let e2_cast = L.build_sitofp e2' num_t "cast" builder in
                       let top_log = Llvm.build_call log_fxn_num [| e2_cast |] "log_func" builder in
                       let bottom_log =  Llvm.build_call log_fxn_num [| e1_cast |] "log_func" builder in
                       let eval_l = Llvm.build_fdiv top_log bottom_log "tmp" builder in
                       L.build_fptosi eval_l i32_t "cast" builder
              | _ -> raise( NotImplemented )
            ) in log_types
        else
        let int_bop op = 
          (match op with
             A.Add     -> L.build_add
           | A.Sub     -> L.build_sub
           | A.Mult    -> L.build_mul
           | A.Div     -> L.build_sdiv
           | A.And     -> L.build_and
           | A.Or      -> L.build_or
           | A.Mod     -> L.build_srem
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
           | A.Leq     -> L.build_icmp L.Icmp.Sle
           | A.Greater -> L.build_icmp L.Icmp.Sgt
           | A.Geq     -> L.build_icmp L.Icmp.Sge
           | _ -> raise( IllegalType )
          ) e1' e2' "tmp" builder in
        let num_bop op = 
          (match op with
             A.Add     -> L.build_fadd
           | A.Sub     -> L.build_fsub
           | A.Mult    -> L.build_fmul
           | A.Div     -> L.build_fdiv
           | A.And     -> L.build_and
           | A.Mod     -> L.build_frem
           | A.Or      -> L.build_or
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
           | A.Leq     -> L.build_icmp L.Icmp.Sle
           | A.Greater -> L.build_icmp L.Icmp.Sgt
           | A.Geq     -> L.build_icmp L.Icmp.Sge
           | _ -> raise( IllegalType )
          ) e1' e2' "tmp" builder in
        let string_of_e1'_llvalue = L.string_of_llvalue e1'
        and string_of_e2'_llvalue = L.string_of_llvalue e2' in

        let space = Str.regexp " " in

        let list_of_e1'_llvalue = Str.split space string_of_e1'_llvalue 
        and list_of_e2'_llvalue = Str.split space string_of_e2'_llvalue in

        let i32_re = Str.regexp "i32\\|32*\\|i8\\|i8\\|i1\\|i1*"
        and num_re = Str.regexp "double\\|double*" in

        let rec match_string regexp str_list i =
          let length = List.length str_list in
          match (Str.string_match regexp (List.nth str_list i) 0) with
            true -> true
          | false -> if ( i> length - 2 ) then false else match_string regexp str_list (succ i) in

        let get_type llvalue = match (match_string i32_re llvalue 0) with
            true -> "int"
          | false -> (match (match_string num_re llvalue 0) with
                true -> "num"
              | false -> "" ) in

        let e1'_type = get_type list_of_e1'_llvalue
        and e2'_type = get_type list_of_e2'_llvalue in

        let build_ops_with_types typ1 typ2 = 
          match (typ1, typ2) with
            "int", "int" -> int_bop op
          | "num", "num" -> num_bop op
          | "num", "int" -> num_bop op
          | "int", "num" -> num_bop op
          | _,_ -> ignore( print_string "Put execption here"); num_bop op
        in
        build_ops_with_types e1'_type e2'_type
      (* end building bin_ops*)

      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
           A.Neg     -> L.build_neg
         | A.Not     -> L.build_not) e' "tmp" builder
      | A.Assign (s, e) -> let e' = expr builder e in ignore( let (s_v, _) = (lookup s) in
                                                              L.build_store e' s_v builder); e'
      | A.Call ("print_int", [e]) | A.Call ("print_bool", [e]) ->
        L.build_call printf_func [| int_format_str ; (expr builder e) |]
          "printf" builder
      | A.Call ("print_num", [e]) ->
        L.build_call printf_func [| float_format_str ; (expr builder e) |]
          "printf" builder
      (* NEW call printf when print_string is called *)
      | A.Call ("print", [e]) ->
        L.build_call printf_func [| str_format_str ; (expr builder e) |]
          "printf" builder
      | A.Call ("printbig", [e]) ->
        L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = (match fdecl.A.typ with A.Void -> ""
                                           | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
            A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
          (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
          (L.build_br merge_bb);

        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
          (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
        L.builder_at_end context merge_bb
      
      | A.Initialize(t, n, e) -> let e' = expr builder e in  
        	let local = L.build_alloca (ltype_of_typ t) n builder in
        	ignore(L.build_store e' local builder);
        	ignore(Hashtbl.add init_hash n (local, t)); builder
 
      | A.For (e1, e2, e3, body) -> stmt builder
                                      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
          A.Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
