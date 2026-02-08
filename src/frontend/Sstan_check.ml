open Core
open Ast
open Middle

type config =
  { protected_vars: String.Set.t
  ; enforce_param_single_use: bool
  ; disallow_sampling_in_control_flow: bool
  ; emit_trusted_loglik: bool }

let reserved_trusted_loglik_name = "sstan_trusted_loglik__"

exception SStanCheckError of Semantic_error.t

let fail ?hint loc message =
  raise (SStanCheckError (Semantic_error.sstan_violation loc message hint))

let sorted_names set = set |> Set.to_list |> List.sort ~compare:String.compare
let string_of_names set = String.concat ~sep:", " (sorted_names set)

let set_from_ids ids =
  List.fold ids ~init:String.Set.empty ~f:(fun acc id -> Set.add acc id.name)

let index_expr_refs = function
  | All -> String.Set.empty
  | Single e | Upfrom e | Downfrom e -> set_from_ids (extract_ids e)
  | Between (e1, e2) ->
      Set.union (set_from_ids (extract_ids e1)) (set_from_ids (extract_ids e2))

let expr_refs (e : typed_expression) = set_from_ids (extract_ids e)

let rec lvalue_index_refs ({lval; _} : typed_lval) =
  match lval with
  | LVariable _ -> String.Set.empty
  | LIndexed (inner, indices) ->
      Set.union (lvalue_index_refs inner)
        (List.fold indices ~init:String.Set.empty ~f:(fun acc idx ->
             Set.union acc (index_expr_refs idx)))
  | LTupleProjection (inner, _) -> lvalue_index_refs inner

let rec lvalue_pack_refs = function
  | LValue lv -> lvalue_index_refs lv
  | LTuplePack {lvals; _} ->
      List.fold lvals ~init:String.Set.empty ~f:(fun acc lv ->
          Set.union acc (lvalue_pack_refs lv))

let truncation_refs truncation =
  Ast.fold_truncation
    (fun acc expr -> Set.union acc (expr_refs expr))
    String.Set.empty truncation

let rec statement_expr_refs (stmt : typed_statement) =
  let sub = statement_expr_refs in
  match stmt.stmt with
  | Assignment {assign_lhs; assign_rhs; _} ->
      Set.union (lvalue_pack_refs assign_lhs) (expr_refs assign_rhs)
  | NRFunApp (_, _, args) ->
      List.fold args ~init:String.Set.empty ~f:(fun acc arg ->
          Set.union acc (expr_refs arg))
  | TargetPE e | JacobianPE e | Return e -> expr_refs e
  | Tilde {arg; args; truncation; _} ->
      List.fold (arg :: args) ~init:(truncation_refs truncation)
        ~f:(fun acc e -> Set.union acc (expr_refs e))
  | IfThenElse (cond, s_true, s_false_opt) ->
      let refs_false =
        Option.value_map s_false_opt ~default:String.Set.empty
          ~f:statement_expr_refs in
      Set.union (expr_refs cond) (Set.union (sub s_true) refs_false)
  | While (cond, body) -> Set.union (expr_refs cond) (sub body)
  | For {lower_bound; upper_bound; loop_body; _} ->
      Set.union (expr_refs lower_bound)
        (Set.union (expr_refs upper_bound) (sub loop_body))
  | ForEach (_, iteratee, loop_body) ->
      Set.union (expr_refs iteratee) (sub loop_body)
  | Profile (_, stmts) | Block stmts ->
      List.fold stmts ~init:String.Set.empty ~f:(fun acc st ->
          Set.union acc (sub st))
  | VarDecl {decl_type; transformation; variables; _} ->
      let refs_in_var acc {initial_value; _} =
        Option.value_map initial_value ~default:acc ~f:(fun e ->
            Set.union acc (expr_refs e)) in
      let refs_in_decl_type =
        SizedType.fold
          (fun acc e -> Set.union acc (expr_refs e))
          String.Set.empty decl_type in
      let refs_in_transform =
        Transformation.fold
          (fun acc e -> Set.union acc (expr_refs e))
          String.Set.empty transformation in
      List.fold variables
        ~init:(Set.union refs_in_decl_type refs_in_transform)
        ~f:refs_in_var
  | Print ps | Reject ps | FatalError ps ->
      List.fold ps ~init:String.Set.empty ~f:(fun acc -> function
        | PString _ -> acc
        | PExpr e -> Set.union acc (expr_refs e))
  | FunDef {body; _} -> sub body
  | Break | Continue | ReturnVoid | Skip -> String.Set.empty

let block_declared_names (block : typed_statement Ast.block option) =
  Ast.get_stmts block
  |> List.fold ~init:String.Set.empty ~f:(fun acc stmt ->
      match stmt.stmt with
      | VarDecl {variables; _} ->
          List.fold variables ~init:acc ~f:(fun acc {identifier; _} ->
              Set.add acc identifier.name)
      | _ -> acc)

let block_loc = function Some {xloc; _} -> xloc | None -> Location_span.empty

let check_reserved_identifier (prog : typed_program) =
  let rec base_lvalue = function
    | {lval= LVariable id; _} -> id
    | {lval= LIndexed (inner, _); _} -> base_lvalue inner
    | {lval= LTupleProjection (inner, _); _} -> base_lvalue inner in
  let rec check_lvalue_pack = function
    | LValue lv ->
        let id = base_lvalue lv in
        if String.equal id.name reserved_trusted_loglik_name then
          fail id.id_loc
            "SStan violation: `sstan_trusted_loglik__` is a reserved \
             identifier in --sstanc mode."
    | LTuplePack {lvals; _} -> List.iter lvals ~f:check_lvalue_pack in
  let check_decl_identifier id =
    if String.equal id.name reserved_trusted_loglik_name then
      fail id.id_loc
        "SStan violation: `sstan_trusted_loglik__` is a reserved identifier in \
         --sstanc mode."
        ~hint:
          "Rename the user-declared identifier; this name is reserved for \
           compiler-generated output." in
  let rec check_stmt (stmt : typed_statement) =
    match stmt.stmt with
    | Assignment {assign_lhs; _} -> check_lvalue_pack assign_lhs
    | VarDecl {variables; _} ->
        List.iter variables ~f:(fun {identifier; _} ->
            check_decl_identifier identifier)
    | For {loop_variable; loop_body; _} ->
        check_decl_identifier loop_variable;
        check_stmt loop_body
    | ForEach (loop_variable, _, loop_body) ->
        check_decl_identifier loop_variable;
        check_stmt loop_body
    | IfThenElse (_, s_true, s_false_opt) ->
        check_stmt s_true;
        Option.iter s_false_opt ~f:check_stmt
    | While (_, body) -> check_stmt body
    | Profile (_, stmts) | Block stmts -> List.iter stmts ~f:check_stmt
    | FunDef {funname; arguments; body; _} ->
        check_decl_identifier funname;
        List.iter arguments ~f:(fun (_, _, arg) -> check_decl_identifier arg);
        check_stmt body
    | Tilde _ | NRFunApp _ | TargetPE _ | JacobianPE _ | Return _ | ReturnVoid
     |Print _ | Reject _ | FatalError _ | Break | Continue | Skip ->
        () in
  Ast.fold_program
    (fun () stmt ->
      check_stmt stmt;
      ())
    () prog

let check_forbidden_target_and_lp (prog : typed_program) =
  let rec check_expr (expr : typed_expression) =
    match expr.expr with
    | GetTarget ->
        fail expr.emeta.loc
          "SStan violation: direct access to `target()` is forbidden in \
           --sstanc mode."
          ~hint:
            "Use distribution sampling statements (`~`) instead of direct \
             target manipulation."
    | FunApp (_, id, args) | CondDistApp (_, id, args) ->
        if String.is_suffix id.name ~suffix:"_lp" then
          fail id.id_loc
            "SStan violation: calls to functions with `_lp` suffix are \
             forbidden in --sstanc mode.";
        List.iter args ~f:check_expr
    | TernaryIf (e1, e2, e3) ->
        check_expr e1;
        check_expr e2;
        check_expr e3
    | BinOp (e1, _, e2) ->
        check_expr e1;
        check_expr e2
    | PrefixOp (_, e)
     |PostfixOp (e, _)
     |Paren e
     |Promotion (e, _)
     |TupleProjection (e, _) ->
        check_expr e
    | Indexed (e, indices) ->
        check_expr e;
        List.iter indices ~f:(function
          | All -> ()
          | Single e | Upfrom e | Downfrom e -> check_expr e
          | Between (e1, e2) ->
              check_expr e1;
              check_expr e2)
    | ArrayExpr es | RowVectorExpr es | TupleExpr es ->
        List.iter es ~f:check_expr
    | Variable _ | IntNumeral _ | RealNumeral _ | ImagNumeral _ -> () in
  let rec check_lvalue ({lval; _} : typed_lval) =
    match lval with
    | LVariable _ -> ()
    | LIndexed (inner, indices) ->
        check_lvalue inner;
        List.iter indices ~f:(function
          | All -> ()
          | Single e | Upfrom e | Downfrom e -> check_expr e
          | Between (e1, e2) ->
              check_expr e1;
              check_expr e2)
    | LTupleProjection (inner, _) -> check_lvalue inner in
  let rec check_lvalue_pack = function
    | LValue lv -> check_lvalue lv
    | LTuplePack {lvals; _} -> List.iter lvals ~f:check_lvalue_pack in
  let rec check_stmt (stmt : typed_statement) =
    match stmt.stmt with
    | Assignment {assign_lhs; assign_rhs; _} ->
        check_lvalue_pack assign_lhs;
        check_expr assign_rhs
    | NRFunApp (_, id, args) ->
        if String.is_suffix id.name ~suffix:"_lp" then
          fail id.id_loc
            "SStan violation: calls to functions with `_lp` suffix are \
             forbidden in --sstanc mode.";
        List.iter args ~f:check_expr
    | TargetPE _ ->
        fail stmt.smeta.loc
          "SStan violation: direct `target +=` adjustments are forbidden in \
           --sstanc mode."
          ~hint:
            "Use distribution sampling statements (`~`) so all score \
             contributions come from vetted probability distributions."
    | JacobianPE e | Return e -> check_expr e
    | Tilde {arg; args; truncation; _} ->
        check_expr arg;
        List.iter args ~f:check_expr;
        ignore
          (Ast.fold_truncation
             (fun () e ->
               check_expr e;
               ())
             () truncation
            : unit)
    | IfThenElse (cond, s_true, s_false_opt) ->
        check_expr cond;
        check_stmt s_true;
        Option.iter s_false_opt ~f:check_stmt
    | While (cond, body) ->
        check_expr cond;
        check_stmt body
    | For {lower_bound; upper_bound; loop_body; _} ->
        check_expr lower_bound;
        check_expr upper_bound;
        check_stmt loop_body
    | ForEach (_, iteratee, loop_body) ->
        check_expr iteratee;
        check_stmt loop_body
    | Profile (_, stmts) | Block stmts -> List.iter stmts ~f:check_stmt
    | VarDecl {decl_type; transformation; variables; _} ->
        ignore
          (SizedType.fold
             (fun () e ->
               check_expr e;
               ())
             () decl_type
            : unit);
        ignore
          (Transformation.fold
             (fun () e ->
               check_expr e;
               ())
             () transformation
            : unit);
        List.iter variables ~f:(fun {initial_value; _} ->
            Option.iter initial_value ~f:check_expr)
    | FunDef {funname; body; _} ->
        if String.is_suffix funname.name ~suffix:"_lp" then
          fail funname.id_loc
            "SStan violation: defining functions with `_lp` suffix is \
             forbidden in --sstanc mode.";
        check_stmt body
    | Print ps | Reject ps | FatalError ps ->
        List.iter ps ~f:(function PString _ -> () | PExpr e -> check_expr e)
    | Break | Continue | ReturnVoid | Skip -> () in
  Ast.fold_program
    (fun () stmt ->
      check_stmt stmt;
      ())
    () prog

type model_state =
  { observed_protected: String.Set.t
  ; protected_counts: int String.Map.t
  ; assigned_params: String.Set.t
  ; param_counts: int String.Map.t }

let update_count counts name =
  Map.update counts name ~f:(function None -> 1 | Some count -> count + 1)

let count_of counts name = Option.value (Map.find counts name) ~default:0

let missing_names counts set =
  Set.filter set ~f:(fun name -> count_of counts name = 0)

let check_preobservation_blocks config (prog : typed_program) =
  let check_block block block_name =
    Ast.get_stmts block
    |> List.iter ~f:(fun stmt ->
        let refs = statement_expr_refs stmt in
        let illegal = Set.inter refs config.protected_vars in
        if not (Set.is_empty illegal) then
          fail stmt.smeta.loc
            (Printf.sprintf
               "SStan violation: protected data referenced in %s before \
                observation: %s."
               block_name (string_of_names illegal))
            ~hint:
              "Move protected-data use into the model block after the matching \
               protected observation statement.") in
  check_block prog.transformeddatablock "transformed data";
  check_block prog.transformedparametersblock "transformed parameters"

let rec lhs_base_identifier (expr : typed_expression) =
  match expr.expr with
  | Variable id -> Some (id, true)
  | Paren e | Promotion (e, _) -> lhs_base_identifier e
  | Indexed (e, _) | TupleProjection (e, _) ->
      Option.map ~f:(fun (id, _) -> (id, false)) (lhs_base_identifier e)
  | _ -> None

let illegal_refs config param_vars state refs =
  let unobserved = Set.diff config.protected_vars state.observed_protected in
  let illegal_protected = Set.inter refs unobserved in
  let illegal_params =
    if config.enforce_param_single_use then
      let unassigned = Set.diff param_vars state.assigned_params in
      Set.inter refs unassigned
    else String.Set.empty in
  (illegal_protected, illegal_params)

let ensure_allowed_refs config param_vars state loc refs =
  let illegal_protected, illegal_params =
    illegal_refs config param_vars state refs in
  if not (Set.is_empty illegal_protected) then
    fail loc
      (Printf.sprintf
         "SStan violation: protected data used before observation: %s."
         (string_of_names illegal_protected))
      ~hint:
        "Observe each protected variable earlier in the model block using `y ~ \
         dist(...)`.";
  if not (Set.is_empty illegal_params) then
    fail loc
      (Printf.sprintf
         "SStan violation: parameter used before its unique prior statement: \
          %s."
         (string_of_names illegal_params))
      ~hint:
        "Move the parameter's sampling statement (`theta ~ dist(...)`) before \
         any use."

let check_distribution_is_builtin (kind : fun_kind) loc =
  match kind with
  | StanLib (FnLpdf _ | FnLpmf _) -> ()
  | StanLib _ ->
      fail loc
        "SStan violation: sampling statements must use a built-in probability \
         distribution."
  | UserDefined _ ->
      fail loc
        "SStan violation: user-defined distributions are not allowed in \
         --sstanc mode."
        ~hint:
          "Use one of Stan's built-in normalized distributions for all \
           sampling statements."

type control_context = {in_loop: bool}

let disallow_sampling_in_control_flow config ~context loc lhs_name =
  if config.disallow_sampling_in_control_flow && context.in_loop then
    fail loc
      (Printf.sprintf
         "SStan violation: sampling statement for `%s` appears inside loop \
          control flow."
         lhs_name)
      ~hint:
        "Move protected observations and parameter priors outside loops. \
         Conditional (`if/else`) sampling is allowed only when branch effects \
         match."

let equal_model_state s1 s2 =
  Set.equal s1.observed_protected s2.observed_protected
  && Map.equal Int.equal s1.protected_counts s2.protected_counts
  && Set.equal s1.assigned_params s2.assigned_params
  && Map.equal Int.equal s1.param_counts s2.param_counts

let ensure_plain_lhs lhs_info loc =
  match lhs_info with
  | Some (_, true) -> ()
  | Some _ ->
      fail loc
        "SStan violation: sampling statement left-hand side must be a plain \
         identifier (no indexing, projection, or expression)."
        ~hint:
          "Use vectorized observation style like `y ~ dist(...)` instead of \
           partial/indexed observations."
  | None ->
      fail loc
        "SStan violation: sampling statement left-hand side must be a variable \
         identifier."

let rec check_model_stmt config param_vars ~context state
    (stmt : typed_statement) =
  match stmt.stmt with
  | Tilde {arg; distribution; kind; args; truncation} ->
      let lhs_info = lhs_base_identifier arg in
      ensure_plain_lhs lhs_info arg.emeta.loc;
      let lhs_id, _ = Option.value_exn lhs_info in
      let lhs_name = lhs_id.name in
      let is_protected = Set.mem config.protected_vars lhs_name in
      let is_param = Set.mem param_vars lhs_name in
      if (not is_protected) && not (config.enforce_param_single_use && is_param)
      then
        fail distribution.id_loc
          (Printf.sprintf
             "SStan violation: sampling statements may only target protected \
              data%s; found `%s`."
             (if config.enforce_param_single_use then " and parameter variables"
              else "")
             lhs_name)
          ~hint:
            "Move deterministic factors out of sampling statements, and keep \
             `~` only for protected data (and parameter priors in strict \
             mode).";
      if is_protected || (config.enforce_param_single_use && is_param) then
        disallow_sampling_in_control_flow config ~context stmt.smeta.loc
          lhs_name;
      check_distribution_is_builtin kind distribution.id_loc;
      let rhs_refs =
        List.fold args ~init:(truncation_refs truncation) ~f:(fun acc e ->
            Set.union acc (expr_refs e)) in
      ensure_allowed_refs config param_vars state stmt.smeta.loc rhs_refs;
      if is_protected then (
        let new_count = count_of state.protected_counts lhs_name + 1 in
        if new_count > 1 then
          fail lhs_id.id_loc
            (Printf.sprintf
               "SStan violation: protected data variable `%s` is observed more \
                than once."
               lhs_name)
            ~hint:
              "Each protected data variable must appear on the left-hand side \
               of exactly one sampling statement.";
        { state with
          observed_protected= Set.add state.observed_protected lhs_name
        ; protected_counts= update_count state.protected_counts lhs_name })
      else
        let new_count = count_of state.param_counts lhs_name + 1 in
        if new_count > 1 then
          fail lhs_id.id_loc
            (Printf.sprintf
               "SStan violation: parameter `%s` has more than one sampling \
                statement in strict mode."
               lhs_name)
            ~hint:
              "Each parameter must have exactly one prior/assignment sampling \
               statement in strict mode.";
        { state with
          assigned_params= Set.add state.assigned_params lhs_name
        ; param_counts= update_count state.param_counts lhs_name }
  | IfThenElse (cond, s_true, s_false_opt) ->
      ensure_allowed_refs config param_vars state cond.emeta.loc
        (expr_refs cond);
      let true_state =
        check_model_stmt config param_vars ~context state s_true in
      let false_state =
        Option.value_map s_false_opt ~default:state ~f:(fun s ->
            check_model_stmt config param_vars ~context state s) in
      if not (equal_model_state true_state false_state) then
        fail stmt.smeta.loc
          "SStan violation: conditional branches must have identical sampling \
           effects for protected data and parameters."
          ~hint:
            "Ensure both branches consume the same protected data and \
             parameters exactly once, or move shared sampling statements \
             outside the conditional.";
      true_state
  | While (cond, body) ->
      ensure_allowed_refs config param_vars state cond.emeta.loc
        (expr_refs cond);
      ignore
        (check_model_stmt config param_vars ~context:{in_loop= true} state body
          : model_state);
      state
  | For {lower_bound; upper_bound; loop_body; _} ->
      ensure_allowed_refs config param_vars state lower_bound.emeta.loc
        (Set.union (expr_refs lower_bound) (expr_refs upper_bound));
      ignore
        (check_model_stmt config param_vars ~context:{in_loop= true} state
           loop_body
          : model_state);
      state
  | ForEach (_, iteratee, loop_body) ->
      ensure_allowed_refs config param_vars state iteratee.emeta.loc
        (expr_refs iteratee);
      ignore
        (check_model_stmt config param_vars ~context:{in_loop= true} state
           loop_body
          : model_state);
      state
  | Block stmts | Profile (_, stmts) ->
      List.fold stmts ~init:state
        ~f:(check_model_stmt config param_vars ~context)
  | FunDef _ ->
      fail stmt.smeta.loc
        "SStan violation: function definitions are not allowed in the model \
         block."
  | _ ->
      ensure_allowed_refs config param_vars state stmt.smeta.loc
        (statement_expr_refs stmt);
      state

let check_model_block config param_vars (prog : typed_program) =
  let initial_counts set =
    Set.fold set ~init:String.Map.empty ~f:(fun acc name ->
        Map.set acc ~key:name ~data:0) in
  let init_state =
    { observed_protected= String.Set.empty
    ; protected_counts= initial_counts config.protected_vars
    ; assigned_params= String.Set.empty
    ; param_counts=
        (if config.enforce_param_single_use then initial_counts param_vars
         else String.Map.empty) } in
  let final_state =
    Ast.get_stmts prog.modelblock
    |> List.fold ~init:init_state
         ~f:(check_model_stmt config param_vars ~context:{in_loop= false}) in
  let missing_protected =
    missing_names final_state.protected_counts config.protected_vars in
  if not (Set.is_empty missing_protected) then
    fail
      (block_loc prog.modelblock)
      (Printf.sprintf
         "SStan violation: each protected data variable must be observed \
          exactly once. Missing observations for: %s."
         (string_of_names missing_protected))
      ~hint:
        "Add one top-level sampling statement for each protected data variable \
         in the model block.";
  if config.enforce_param_single_use then
    let missing_params = missing_names final_state.param_counts param_vars in
    if not (Set.is_empty missing_params) then
      fail
        (block_loc prog.modelblock)
        (Printf.sprintf
           "SStan violation: each parameter must have exactly one sampling \
            statement in strict mode. Missing priors for: %s."
           (string_of_names missing_params))
        ~hint:
          "Add one top-level sampling statement for each parameter in the \
           model block."

let check_program config (prog : typed_program) =
  try
    let data_vars = block_declared_names prog.datablock in
    let param_vars = block_declared_names prog.parametersblock in
    let unknown_protected = Set.diff config.protected_vars data_vars in
    if not (Set.is_empty unknown_protected) then
      fail (block_loc prog.datablock)
        (Printf.sprintf
           "SStan violation: --sstan-protect contains names not declared as \
            top-level data variables: %s."
           (string_of_names unknown_protected))
        ~hint:
          "Ensure every protected name is declared in the data block, or \
           remove it from --sstan-protect.";
    check_reserved_identifier prog;
    check_forbidden_target_and_lp prog;
    check_preobservation_blocks config prog;
    check_model_block config param_vars prog;
    Ok ()
  with SStanCheckError err -> Error err
