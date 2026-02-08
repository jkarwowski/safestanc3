type config =
  { protected_vars: Core.String.Set.t
  ; enforce_param_single_use: bool
  ; disallow_sampling_in_control_flow: bool
  ; emit_trusted_loglik: bool }

val check_program :
  config -> Ast.typed_program -> (unit, Semantic_error.t) result
