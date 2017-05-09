(*Code generation: translate takes a semantically checked AST and
   produces LLVM IR
   LLVM tutorial: Make sure to read the OCaml version of the tutorial
   http://llvm.org/docs/tutorial/index.html
   Detailed documentation on the OCaml LLVM library:
   http://llvm.moe/
   http://llvm.moe/ocaml/
*)

module L = Llvm
module AST = Ast
module A = Sast (* I'm going to replace AST with SAST
                  but keep var name A for ease *)

module StringMap = Map.Make(String)
exception NotImplemented
exception IllegalType


let translate (program_unit_list) =
  let context = L.global_context () in
  let the_module = L.create_module context "damo"
  and i32_t  = L.i32_type  context
  and num_t = L.double_type context
  and i8_t   = L.i8_type context
  and str_t = L.pointer_type (L.i8_type context)
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context
  (*and void_ptr = L.pointer_type (L.i8_type context)*)
  and symbol_t = L.pointer_type (L.i8_type context) in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Num -> num_t
    | A.String -> str_t
    | A.Symbol -> symbol_t
    | A.Void -> void_t in


  (* Declare each global variable; remember its value in a map *)

  (* 
    The SAST comes in as a list of variable declarations, statements and
    function declarations. The following function is there to parse this
    list into its three component lists: 
    a list for main fxn statements,
    a list for function declarations, 
    and a list for declarations 
  *)
  let rec combine_prog_units prog_stmts main_stmts gvars func_unit = match prog_stmts with
        [] -> (main_stmts, gvars, func_unit)
      | A.VarUnit(b)::tail -> (combine_prog_units tail main_stmts (gvars@[b]) func_unit)
      | A.FuncUnit(f)::tail -> (combine_prog_units tail main_stmts gvars (func_unit@[f]))
      | A.StmtUnit(s)::tail -> (combine_prog_units tail (main_stmts@[s]) gvars func_unit)

      in
 
  let topstmts, gvars, functions = combine_prog_units program_unit_list [] [] [] in

  let main_function = {
    A.s_typ = A.Int;
    A.s_fname = "main";
    A.s_formals = [];
    A.s_body = List.map (fun x -> A.StmtUnit(x)) topstmts; (*filtered_main_stmts;*)
  } in
  
  let array_hash = Hashtbl.create 20 in 
  let init_hash = Hashtbl.create 20 in 

  (* Now build up global variables by parsing the list of globals
      First parse the decls in s_bind:
      Here is where I'm making an assumption, I assume that InitDecl's
      expr will be a Literal (not ID) *)
  
  let global_vars = 
    let global_var m decl = 
      (match decl with
          A.Decl( t, s ) -> 
              (match t with
                   A.Int -> let init = L.const_int (ltype_of_typ t) 0 in
                    StringMap.add s ((L.define_global s init the_module),t) m
                 | A.Num -> let init = L.const_float (ltype_of_typ t) 0.0 in
                    StringMap.add s ((L.define_global s init the_module),t) m
                 | A.String -> let init = L.const_pointer_null (ltype_of_typ t) in
                    StringMap.add s ((L.define_global s init the_module),t) m
                 | A.Bool -> let init = L.const_int (ltype_of_typ t) 0 in
                    StringMap.add s ((L.define_global s init the_module),t) m
                 | A.Symbol -> let init = L.const_pointer_null (*(L.i8_type context)*) symbol_t in 
                    StringMap.add s ((L.define_global s init the_module), t) m
                 | _ -> let init = L.const_int (ltype_of_typ t) 0 in
                    StringMap.add s ((L.define_global s init the_module),t) m)
        (*| A.ArrDecl (t, s, el) -> let init = L.const_pointer_null (L.i8_type context) in 
		    Hashtbl.add array_hash s ((((L.define_global s init the_module), t) m), el)
	*)|   _ -> raise(Failure("Illegal Type"))
      )
  in List.fold_left global_var StringMap.empty gvars in 
(*
  let _ = Printf.printf "%s" (String.concat "\n" (List.map A.string_of_stmt main_function.A.body)) in
  *)

  let functions = main_function :: functions in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in
  let string_compare = L.declare_function "strcmp" 
            ( Llvm.function_type i32_t [| str_t; str_t |]) the_module in

  let symbol_operator = Llvm.declare_function "operator" 
            ( Llvm.function_type str_t [| symbol_t |] ) the_module in
  let symbol_malloc = Llvm.declare_function "createSymbol" 
            ( Llvm.function_type symbol_t [|  |] ) the_module in
  let symbol_const_check = Llvm.declare_function "isConstant" 
            ( Llvm.function_type i32_t [| symbol_t |] ) the_module in
  let root_symbol = Llvm.declare_function "createRoot"
            ( Llvm.function_type symbol_t [| symbol_t; symbol_t; str_t; |])  the_module in
  let const_symbol = Llvm.declare_function "setSymbolValue" 
            ( Llvm.function_type symbol_t [| symbol_t; num_t |] ) the_module in
  let value_symbol = Llvm.declare_function "value" 
            ( Llvm.function_type num_t [| symbol_t |] ) the_module in
  let symbol_init_check = Llvm.declare_function "isInitialized" 
            ( Llvm.function_type i32_t [| symbol_t |] ) the_module in
  let symbol_left = Llvm.declare_function "left" 
            ( Llvm.function_type symbol_t [| symbol_t |] ) the_module in
  let symbol_right = Llvm.declare_function "right" 
            ( Llvm.function_type symbol_t [| symbol_t |] ) the_module in
  (*let malloc = LLvm.declare_function "malloc"
	    ( Llvm.function_type void_ptr [| i_32 |] ) the_module in  
  *)
  let pwr_fxn_num = Llvm.declare_function "pow" ( Llvm.function_type num_t [| num_t; num_t |] ) the_module in
  let log_fxn_num = Llvm.declare_function "log" ( Llvm.function_type num_t [|  num_t |] ) the_module in
  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let check_type = function 
        A.Decl(t,_) -> ltype_of_typ t 
      | A.ArrDecl(_,_,_) -> i8_t 
      in 
      let name = fdecl.A.s_fname
      and formal_types =
        Array.of_list (List.map (fun x -> check_type x) fdecl.A.s_formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.s_typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.s_fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* NEW formatting string for using printf on strings *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmtint" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmtstr" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "floatstr" builder in

    (* 
      PREPROCESSING STEP: PARSE FUNCTION BODY INTO VARFUNIT AND STMTFUNIT,
        - GET LIST OF VARFUNIT TO BE PROCESSED BY LOCAL VARS TO GET LOCALS
    *)
    let rec fxn_body_decouple f_body decl_l stmt_l  = (match f_body with
        [] -> (decl_l, stmt_l)
      | A.VarUnit(s) :: tail -> fxn_body_decouple tail (decl_l@[s]) stmt_l
      | A.StmtUnit(sf) :: tail -> fxn_body_decouple tail decl_l (stmt_l@[sf])
      | _ :: _ -> raise(Failure("Nothing else should be in a function body"))
    ) 
    in 

    let locals, stmt_list = fxn_body_decouple fdecl.A.s_body [] [] in 
    let lookup_global n = 
          try StringMap.find n global_vars
          with Not_found -> raise(Failure("In add main, variable not defined")) in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m x p = (match x with
          A.Decl(t, n) -> (match t with 
            A.Symbol -> L.set_value_name n p; 
                      let local = L.build_alloca (ltype_of_typ t) n builder in
                      ignore (L.build_store p local builder);
                      StringMap.add n (local, t) m 
            | _ -> L.set_value_name n p; let local = L.build_alloca (ltype_of_typ t) n builder in
                      ignore (L.build_store p local builder);
                      StringMap.add n (local, t) m )
        | A.ArrDecl(t,n,_) -> L.set_value_name n p;  
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n (local, t) m )
    in
      let add_local m x= (match x with 
          A.Decl(t, n) -> (match t with
              A.Symbol -> let variable = L.build_call symbol_malloc [| |] "symbolmal" builder in
                let v_ptr = symbol_t in let s_ptr= L.build_alloca v_ptr n builder 
                in ignore(L.build_store variable s_ptr builder);
                StringMap.add n (s_ptr, t) m 

              | _ -> let local_var = L.build_alloca (ltype_of_typ t) n builder
              in StringMap.add n (local_var, t) m
            )
          | A.ArrDecl(t, n, _) -> let local_var = L.build_alloca (ltype_of_typ t) n builder
            in StringMap.add n (local_var, t) m 
      ) in
      (* Most global variables were declared at the top, but side i didn't have a builder for
        the global context, this fuction mallocs all the global symbols and adds back to the 
        global map. all other variables are left unchanged *)
      let add_main_local m x = (match x with 
          A.Decl(t, n) -> (match t with
            A.Symbol -> let global_variable = L.build_call symbol_malloc [| |] "symbolmal" builder in
                let (s_v, _) = lookup_global n in ignore(L.build_store global_variable s_v builder);
                m
            | _ -> m
          )
          | A.ArrDecl(t, n, _) -> let local_var = L.build_alloca (ltype_of_typ t) n builder
            in StringMap.add n (local_var, t) m 
      ) in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.s_formals
          (Array.to_list (L.params the_function)) in
      if fdecl.A.s_fname = "main" then 
          List.fold_left add_main_local global_vars gvars
      else
          List.fold_left add_local formals locals in
        (* Return the value for a variable or formal argument *)
    let lookup n = 
      if fdecl.A.s_fname = "main" then 
          try StringMap.find n global_vars
          with Not_found -> raise(Failure("Global variable not defined"))
      else
        try StringMap.find n local_vars
        with Not_found -> try StringMap.find n global_vars
        with Not_found -> try Hashtbl.find init_hash n
        with Not_found -> raise (Failure ("not defined yet")) 
    in
      let get_type node = (match node with
              A.IntLit (t , _) -> t
            | A.BoolLit (t , _) -> t
            | A.StringLit (t ,_) -> t
            | A.NumLit (t,_ ) -> t 
            | A.Id (t, _) -> t
            | A.ArrId (t, _, _) -> t
            | A.Binop (t, _, _, _) -> t
            | A.Unop (t, _, _) -> t
            | A.Assign (lvalue, _) -> (match lvalue with 
               A.Idl(t, _) -> t
              | A.ArrIdl(t, _, el) -> t)  
            | A.Call (t, _ , _) -> t
            (*| A.Indexing (t, _, _) -> t*)
            | A.Noexpr (t) -> t
        ) in
      let get_str_op op_t = (match op_t with 
             AST.Add     -> "PLUS"
           | AST.Sub     -> "MINUS"
           | AST.Mult    -> "TIMES"
           | AST.Div     -> "DIVIDE"
           | AST.Exp     -> "EXP"
           | AST.Log     -> "LOG"
           | _ -> raise(Failure("Not supported operator"))
        ) in
    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit(t, i) -> L.const_int i32_t i 
      | A.BoolLit(t, b) -> L.const_int i1_t (if b then 1 else 0)
      (* NEW for creating strings from expressions *)
      | A.StringLit(t, st) -> L.build_global_stringptr st "tmp" builder 
      | A.NumLit(t, num) -> L.const_float num_t num
      | A.Noexpr(t) -> L.const_int i32_t 0
      | A.Id(t, s) -> let (s_v,_) = lookup s in L.build_load s_v s builder
      (*| A.Indexing (t,n, e) -> let (pointer,_) = Hashtbl.find array_hash n in
		let e' = expr builder e in 
		L.build_load (L.build_gep pointer [| e' |] "tmp" builder) "val" builder
      *)		  
      | A.Binop (t, e1, op, e2) ->
        let get_name e_xp = (match e_xp with
          A.Id( _, n ) -> n
          | _ -> raise(Failure("WHY IS THIS NOT AN ID"))
        ) in
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        let t_1 = get_type( e1 )
        and t_2 = get_type( e2 ) in
        let binop_type_check ( t, e1, e2 ) = 
           (match t with
             A.Int -> (e1', e2')
            |A.Bool -> (match t_1, t_2 with 
                  A.Num, A.Num -> (e1', e2')
                | A.Num, A.Int -> (e1', 
                    L.build_sitofp e2' num_t "cast" builder)
                | A.Int, A.Num -> (L.build_sitofp e1' num_t "cast" builder, 
                    e2')
                | A.Bool, A.Bool -> (e1', e2')
                | A.Int, A.Int -> (e1', e2')
                | A.Symbol, A.Symbol -> (e1', e2')
                | _ -> raise(Failure("not matched")))
            |A.Num -> (match t_1, t_2 with 
                  A.Num, A.Num -> (e1', e2')
                | A.Num, A.Int -> (e1', 
                    L.build_sitofp e2' num_t "cast" builder)
                | A.Int, A.Num -> (L.build_sitofp e1' num_t "cast" builder, 
                    e2')
                | A.Bool, A.Bool -> (e1', e2')  
                | _ -> raise(Failure("not matched num")))
            |A.Symbol -> let t1 = get_type e1 and t2 = (get_type e2) in ( match t1, t2 with
                (* if they are both symbols, then get their names and look them up to
                    get back llvm values *)
                  A.Symbol, A.Symbol -> e1', e2'
                | A.Num, A.Symbol -> let num_node = (L.build_call symbol_malloc [| |] "symbolmal" builder) in
                      let e' = L.build_call const_symbol [| num_node; e1' |] "symbolm" builder in (e', e2')
                | A.Symbol, A.Num -> let num_node = (L.build_call symbol_malloc [| |] "symbolmal" builder) in
                      let e' = L.build_call const_symbol [| num_node; e2' |] "symbolm" builder in (e1', e')
                | A.Int, A.Symbol -> let num_node = (L.build_call symbol_malloc [| |] "symbolmal" builder) in
                      let e' = L.build_call const_symbol [|num_node; (L.build_sitofp e1' num_t "cast" builder) |] 
                      "symbolm" builder in (e', e2')
                | A.Symbol, A.Int -> let num_node = (L.build_call symbol_malloc [| |] "symbolmal" builder) in
                      let e' = L.build_call const_symbol [|num_node; (L.build_sitofp e2' num_t "cast" builder) |] 
                      "symbolm" builder in (e1', e')
                | _, _ -> raise(Failure("Not here yet"))
                )
            | _ -> raise(Failure("Shouldn't be here"))
          )
           in
        let e1_new', e2_new' = binop_type_check(t, e1, e2) in 

        if t = A.Symbol then let sym_type =  
            let operat = L.build_global_stringptr (get_str_op(op)) "tmp" builder in 
            let e' = L.build_call root_symbol [| e1_new'; e2_new'; operat |] "symbolm" builder in  e'
            in sym_type
        (* Exp and Log are run time functions because they are without llvm equivalents *)
        else if op = AST.Exp then 
        (* For exp functions, if num just call the exp function, else cast int to num
        then build exp funtion then cast back to int *)
            let exp_types = (match t with
              A.Num -> Llvm.build_call pwr_fxn_num [| e1_new'; e2_new' |] "pow_func" builder
            | A.Int -> (* Power fxn only defined in terms of floats*)
               let e1_cast = L.build_sitofp e1_new' num_t "cast" builder in
               let e2_cast = L.build_sitofp e2_new' num_t "cast" builder in
               let pow_cast =  Llvm.build_call pwr_fxn_num [| e1_cast; e2_cast |] "pow_func" builder in
               L.build_fptosi pow_cast i32_t "cast" builder
            | _ -> raise( NotImplemented )
            ) in exp_types
        else if op = AST.Log then
        (* For log functions, if num, then get log_e of the num, then log_e of the base
        then divide them using llvm divide fxn
        for int, first convert, then do the above, then convert back *)
         let log_types = (match t with
              A.Num -> let top_log = Llvm.build_call log_fxn_num [| e2_new' |] "log_func" builder in
                       let bottom_log =  Llvm.build_call log_fxn_num [| e1_new' |] "log_func" builder in
                       Llvm.build_fdiv top_log bottom_log "tmp" builder
              | A.Int -> 
                       let e1_cast = L.build_sitofp e1_new' num_t "cast" builder in
                       let e2_cast = L.build_sitofp e2_new' num_t "cast" builder in
                       let top_log = Llvm.build_call log_fxn_num [| e2_cast |] "log_func" builder in
                       let bottom_log =  Llvm.build_call log_fxn_num [| e1_cast |] "log_func" builder in
                       let eval_l = Llvm.build_fdiv top_log bottom_log "tmp" builder in
                       L.build_fptosi eval_l i32_t "cast" builder
              | _ -> raise( NotImplemented )
            ) in log_types
        else
        let int_bop op = 
          (match op with
             AST.Add     -> L.build_add
           | AST.Sub     -> L.build_sub
           | AST.Mult    -> L.build_mul
           | AST.Div     -> L.build_sdiv
           | AST.And     -> L.build_and
           | AST.Or      -> L.build_or
           | AST.Mod     -> L.build_srem
           | AST.Equal   -> L.build_icmp L.Icmp.Eq
           | AST.Neq     -> L.build_icmp L.Icmp.Ne
           | AST.Less    -> L.build_icmp L.Icmp.Slt
           | AST.Leq     -> L.build_icmp L.Icmp.Sle
           | AST.Greater -> L.build_icmp L.Icmp.Sgt
           | AST.Geq     -> L.build_icmp L.Icmp.Sge
           | _ -> raise( IllegalType )
          ) e1_new' e2_new' "tmp" builder in
        let num_bop op = 
          (match op with
             AST.Add     -> L.build_fadd
           | AST.Sub     -> L.build_fsub
           | AST.Mult    -> L.build_fmul
           | AST.Div     -> L.build_fdiv
           | AST.And     -> L.build_and
           | AST.Mod     -> L.build_frem
           | AST.Or      -> L.build_or
           | AST.Equal   -> L.build_fcmp L.Fcmp.Oeq
           | AST.Neq     -> L.build_fcmp L.Fcmp.One
           | AST.Less    -> L.build_fcmp L.Fcmp.Olt
           | AST.Leq     -> L.build_fcmp L.Fcmp.Ole
           | AST.Greater -> L.build_fcmp L.Fcmp.Ogt
           | AST.Geq     -> L.build_fcmp L.Fcmp.Oge
           | _ -> raise( IllegalType )
          ) e1_new' e2_new' "tmp" builder in 
	let symbol_bop op = 
	(match op with 
	     AST.Equal -> L.build_ptrdiff
	) e1_new' e2_new' "tmp" builder in 
        let build_ops_with_types t = 
          (match (t) with
            A.Int -> int_bop op
          | A.Num -> num_bop op
          | A.Bool -> match t_1, t_2 with 
                A.Num, A.Num -> num_bop op
              | A.Int, A.Num -> num_bop op
              | A.Num, A.Int -> num_bop op
              | A.Int, A.Int -> int_bop op
              | A.Symbol, A.Symbol -> L.build_icmp L.Icmp.Eq (L.build_pointercast e1_new' i32_t "e1" builder) (L.build_pointercast e2_new' i32_t "e2" builder) "tmp" builder
              | _, _ -> raise(Failure("Not supported for boolean ops, for strings use strcmp"))
          (*why did you have the extra underscore*)
          | _-> ignore( print_string "build op exception"); num_bop op
          )     
  	   in (build_ops_with_types t) (*e1_new' e2_new'*)
      (* end building bin_ops*)

      | A.Unop(t, op, e) ->
        let e' = expr builder e in
        (match op with
           AST.Neg -> L.build_neg
         | AST.Not -> L.build_not) e' "tmp" builder
      | A.Assign (A.Idl(t, s), e) -> (match t with
                      A.Symbol -> let t_1 = get_type e in
                        (match t_1 with 
                          A.Num -> let e_val = expr builder e in
                            let (s_v, _) = (lookup s) in let s_v1 = L.build_load s_v s builder in
                            let e' = L.build_call const_symbol [| s_v1; e_val |] "symbolm" builder in
                            ignore( L.build_store e' s_v builder); e'
                        | A.Int -> let e_val = L.build_sitofp (expr builder e) num_t "cast" builder in
                            let (s_v, _) = (lookup s) in let s_v1  =  L.build_load s_v s builder in
                            let e' = L.build_call const_symbol [| s_v1; e_val |] "symbolm" builder in
                            ignore( L.build_store e' s_v builder); e'
                        | _ -> let e_val = expr builder e in 
                            let (s_v, _) = lookup s in 
                            ignore( L.build_store e_val s_v builder ); e_val )
                      | A.Num -> let t_1 = get_type e in
                            (match t_1 with 
                              A.Num -> let e_val = expr builder e in ignore( let (s_v, _) = (lookup s) in 
                                                              L.build_store e_val s_v builder); e_val
                            | A.Int -> let e_val = (L.build_sitofp (expr builder e) num_t "cast" builder) in
                                          ignore( let (s_v, _) = (lookup s) in 
                                          L.build_store e_val s_v builder); e_val
                            | _ -> raise(Failure("No num should have anything but a num or int assigned to it"))
                          )
                      | _ -> let e' = expr builder e in ignore( let (s_v, _) = (lookup s) in 
                                                              L.build_store e' s_v builder); e')


      | A.Assign (A.ArrIdl(t, s, el), e) -> let e' = expr builder e in ignore( let (s_v, _) = (lookup s) in
                                                              L.build_store e' s_v builder); e' 
      | A.Call (t, "print_int", [e]) | A.Call (t, "print_bool", [e]) ->
        L.build_call printf_func [| int_format_str ; (expr builder e) |]
          "printf" builder
      | A.Call (t, "strcompare", [e1; e2]) -> L.build_call string_compare [| (expr builder e1); (expr builder e2) |]
          "strcmp" builder


      (* Symbol function calls *)
      | A.Call (_, "value", [e]) ->
        L.build_call value_symbol [|  (expr builder e) |]
          "symbol_value" builder
      | A.Call (_, "isConstant", [e]) ->
        L.build_call symbol_const_check [|  (expr builder e) |]
          "symbol_const" builder
      | A.Call (_, "isInitialized", [e]) ->
        L.build_call symbol_init_check [|  (expr builder e) |]
          "symbol_init" builder
      | A.Call (_, "left", [e]) ->
        L.build_call symbol_left [|  (expr builder e) |]
          "symbol_left_call" builder
      | A.Call (_, "right", [e]) ->
        L.build_call symbol_right [|  (expr builder e) |]
          "symbol_right_call" builder
      | A.Call (_, "operator", [e]) ->
        L.build_call symbol_operator [|  (expr builder e) |]
          "symbol_operator" builder

      | A.Call (_, "print_num", [e]) ->
        L.build_call printf_func [| float_format_str ; (expr builder e) |]
          "printf" builder
      (* NEW call printf when print_string is called *)
      | A.Call (_, "print", [e]) ->
        L.build_call printf_func [| str_format_str ; (expr builder e) |]
          "printf" builder
      | A.Call (_, "printbig", [e]) ->
        L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | A.Call (_, f, act) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = (match fdecl.A.s_typ with A.Void -> ""
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
        A.Block(sl) -> List.fold_left stmt builder sl
      | A.Expr(e) -> ignore (expr builder e); builder
      | A.Return(e) -> 
	ignore (match fdecl.A.s_typ with
            A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder
	); builder

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
    
      | A.For(e1, e2, e3, body) -> stmt builder
					( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ])
    
    in 
    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block stmt_list) in 
        add_terminal builder (match fdecl.A.s_typ with
          A.Void -> L.build_ret_void
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in 
  
  List.iter build_function_body functions; the_module


