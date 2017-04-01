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

(*
Figuring out printing:
Std lib function-->
let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
let _ = declare_function "printf" printf_ty the_module


so declaring functions:
get the next function (encoded into llvm) from string map, get function values
grab basic function builder block, add the function name to context
then, get pointer to position immediately after function name in
memory, add formal arguments to function in memory

now int_formet_str should be pointing to the beginning of formal
args in memorys

build_global_stringptr

*)




let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "damo"
  and i32_t  = L.i32_type  context
  and num_t = L.double_type context
  and i8_t   = L.i8_type context
  and str_t = L.pointer_type (L.i8_type context)
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Num -> num_t
    | A.String -> str_t
    | A.Void -> void_t in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n ((L.define_global n init the_module),t) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

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

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
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
           | A.Literal i , A.NumLit n -> (expr builder (A.NumLit (float_of_int (i))), e2')
           | A.NumLit n, A.Literal i -> (e1', expr builder (A.NumLit (float_of_int (i)) ))
           | A.Id(n), A.Id(i) -> let _, t1 = lookup n in 
             let _, t2 = lookup i in
             if (t1 = A.Num) && (t2 = A.Int) then 
               e1', (L.build_sitofp e2' num_t "cast" builder)
             else if (t1 = A.Int) && (t2 = A.Num) then 
               (L.build_sitofp e1' num_t "cast" builder), e2'
             else
               e1', e2'
           | A.Id(n), A.Literal(i) -> let _, t1 = lookup n in 
              if t1 = A.Num then 
                e1', (L.build_sitofp e2' num_t "cast" builder)
              else
                e1', e2'
           | A.Literal(i), A.Id(n) -> let _, t1 = lookup n in 
              if t1 = A.Num then 
                (L.build_sitofp e1' num_t "cast" builder), e2'
              else
                e1', e2'
            | A.Id(i), A.NumLit(n) -> let _, t1 = lookup i in 
              if t1 = A.Int then 
                (L.build_sitofp e1' num_t "cast" builder), e2'
              else
                e1', e2'
            | A.NumLit(i), A.Id(n) -> let _, t1 = lookup n in 
              if t1 = A.Int then 
                e1', (L.build_sitofp e2' num_t "cast" builder)
              else
                e1', e2'
           | _, _ -> e1', e2'
          ) in
        (* if we have int+num, cast int into a float and continue*)
        let (e1', e2') = bop_int_with_num (e1, e2)

        in
        let int_bop op = 
          (match op with
             A.Add     -> L.build_add
           | A.Sub     -> L.build_sub
           | A.Mult    -> L.build_mul
           | A.Div     -> L.build_sdiv
           | A.And     -> L.build_and
           | A.Or      -> L.build_or
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
           | A.Leq     -> L.build_icmp L.Icmp.Sle
           | A.Greater -> L.build_icmp L.Icmp.Sgt
           | A.Geq     -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder in
        let num_bop op = 
          (match op with
             A.Add     -> L.build_fadd
           | A.Sub     -> L.build_fsub
           | A.Mult    -> L.build_fmul
           | A.Div     -> L.build_fdiv
           | A.And     -> L.build_and
           | A.Or      -> L.build_or
           | A.Equal   -> L.build_icmp L.Icmp.Eq
           | A.Neq     -> L.build_icmp L.Icmp.Ne
           | A.Less    -> L.build_icmp L.Icmp.Slt
           | A.Leq     -> L.build_icmp L.Icmp.Sle
           | A.Greater -> L.build_icmp L.Icmp.Sgt
           | A.Geq     -> L.build_icmp L.Icmp.Sge
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
