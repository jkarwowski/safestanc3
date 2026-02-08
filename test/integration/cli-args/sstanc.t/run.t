Cmdliner error output can be different if color is enabled
  $ export NO_COLOR=1

Require protected list
  $ stanc --sstanc pass/01_basic_scalar_normal.stan
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: SStan mode requires --sstan-protect
  [124]

Reject non-data protected names
  $ stanc --sstanc --sstan-protect=not_data pass/01_basic_scalar_normal.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err

PASS cases compile
  $ stanc --sstanc --sstan-protect=y pass/01_basic_scalar_normal.stan
  $ stanc --sstanc --sstan-protect=y pass/02_vectorized_linear_regression.stan
  $ stanc --sstanc --sstan-protect=y,z pass/03_two_protected_scalars_after_observation.stan
  $ stanc --sstanc --sstan-protect=y pass/04_hierarchical_group_index_loop.stan
  $ stanc --sstanc --sstan-protect=y pass/05_logistic_regression_bernoulli_logit.stan
  $ stanc --sstanc --sstan-protect=y pass/06_poisson_regression_log_link.stan
  $ stanc --sstanc --sstan-protect=y pass/07_multivariate_normal_likelihood.stan
  $ stanc --sstanc --sstan-protect=y pass/08_robust_student_t_regression.stan
  $ stanc --sstanc --sstan-protect=x_obs,y_obs pass/09_error_in_variables_two_protected_vectors.stan
  $ stanc --sstanc --sstan-protect=y pass/10_udf_deterministic_helper.stan

FAIL cases reject with SStan violation
  $ stanc --sstanc --sstan-protect=y fail/01_arbitrary_scoring_target_plus_equals.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/02_protected_data_used_before_observation.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/03_protected_data_in_transformed_data.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/04_protected_data_never_observed.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/05_protected_data_observed_twice.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/06_partial_observation_indexed_lhs.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/07_observation_inside_control_flow.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/08_user_defined_distribution_in_tilde.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/09_sampling_on_unprotected_data.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/10_lp_function_definition_or_call.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err

Legacy pass cases compile from new layout
  $ stanc --sstanc --sstan-protect=y pass/good.stan
  $ stanc --sstanc --sstan-protect=x,y pass/good-multi-protected.stan
  $ stanc --sstanc --sstan-protect=y pass/good-unprotected-covariate.stan

Legacy fail cases reject from new layout
  $ stanc --sstanc --sstan-protect=y fail/bad-target.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-lp-fundef.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-unobserved.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-double-observe.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-transformed-data-use.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-before-observe.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-indexed-observe.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-userdist.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y fail/bad-control-flow-observe.stan > /tmp/sstanc.err 2>&1; status=$?; test $status -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err

Compared benchmark models: originals compile in standard Stan mode
  $ stanc compared/arK.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/arma.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/eight_schools.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/gp_pois_regr.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/gp_regr.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/irt_2pl.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/one_comp_mm_elim_abs.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/sir.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/low_dim_gauss_mix.original.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc compared/low_dim_gauss_mix_collapse.original.stan >/tmp/sstanc_compile.log 2>&1

Compared benchmark models: SafeStan transplants that pass
  $ stanc --sstanc --sstan-protect=y_obs compared/arK.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=y_obs compared/arma.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=y compared/eight_schools.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=k compared/gp_pois_regr.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=y compared/gp_regr.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=y_flat compared/irt_2pl.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=C_hat compared/one_comp_mm_elim_abs.safestan.stan >/tmp/sstanc_compile.log 2>&1
  $ stanc --sstanc --sstan-protect=stoi_hat,B_hat compared/sir.safestan.stan >/tmp/sstanc_compile.log 2>&1

Compared benchmark models: known non-transplantable cases fail under SafeStan
  $ stanc --sstanc --sstan-protect=y compared/low_dim_gauss_mix.safestan.stan > /tmp/sstanc.err 2>&1; rc=$?; test $rc -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ grep -q "direct" /tmp/sstanc.err
  $ grep -q "target +=" /tmp/sstanc.err
  $ stanc --sstanc --sstan-protect=y compared/low_dim_gauss_mix_collapse.safestan.stan > /tmp/sstanc.err 2>&1; rc=$?; test $rc -ne 0
  $ grep -q "SStan violation:" /tmp/sstanc.err
  $ grep -q "direct" /tmp/sstanc.err
  $ grep -q "target +=" /tmp/sstanc.err
