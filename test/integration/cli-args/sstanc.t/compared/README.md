# SafeStan Comparison Set (10 benchmark-style models)

Each model has two files:
- `*.original.stan`: copied from `test/integration/good/stat_comp_benchmarks_models`
- `*.safestan.stan`: SafeStan transplant (or an intentionally failing attempt)

## Status summary

- `arK`: transplanted by splitting response into protected `y_obs` and lag source `y_hist`.
- `arma`: transplanted by splitting response into protected `y_obs` and lag source `y_hist`.
- `eight_schools`: transplanted unchanged.
- `gp_pois_regr`: transplanted unchanged.
- `gp_regr`: transplanted by moving parameter priors before parameter use in model-local declarations.
- `irt_2pl`: transplanted by flattening protected outcomes to `y_flat` for a single vectorized likelihood statement.
- `one_comp_mm_elim_abs`: transplanted by vectorizing the observation statement to avoid indexed lhs sampling.
- `sir`: transplanted by vectorizing `stoi_hat` likelihood into one top-level statement.
- `low_dim_gauss_mix`: not transplantable under current SafeStan due required `target += log_mix(...)`.
- `low_dim_gauss_mix_collapse`: not transplantable under current SafeStan due required `target += log_mix(...)`.

## Notes on equivalence

Some transplants require data reshaping/duplication (`y_obs` + `y_hist` or flattened arrays).
They preserve the original log-density only when those duplicated/reshaped data are consistent
with the original data representation.
