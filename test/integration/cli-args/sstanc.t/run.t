Cmdliner error output can be different if color is enabled
  $ export NO_COLOR=1

Require protected list
  $ stanc --sstanc good.stan
  Usage: %%NAME%% [--help] [OPTION]… [MODEL_FILE]
  %%NAME%%: SStan mode requires --sstan-protect
  [124]

Reject target edits
  $ stanc --sstanc --sstan-protect=y bad-target.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: direct `target +=` adjustments are forbidden in --sstanc mode.

Reject _lp function definitions
  $ stanc --sstanc --sstan-protect=y bad-lp-fundef.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: defining functions with `_lp` suffix is forbidden in --sstanc mode.

Reject missing protected observations
  $ stanc --sstanc --sstan-protect=y bad-unobserved.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: each protected data variable must be observed exactly once. Missing observations for: y.

Reject duplicate protected observations
  $ stanc --sstanc --sstan-protect=y bad-double-observe.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: protected data variable `y` is observed more than once.

Reject protected data in transformed data
  $ stanc --sstanc --sstan-protect=y bad-transformed-data-use.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: protected data referenced in transformed data before observation: y.

Reject protected data use before observation in model block
  $ stanc --sstanc --sstan-protect=y bad-before-observe.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: protected data used before observation: y.

Reject indexed protected observation
  $ stanc --sstanc --sstan-protect=y bad-indexed-observe.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: sampling statement left-hand side must be a plain identifier (no indexing, projection, or expression).

Reject user-defined distributions
  $ stanc --sstanc --sstan-protect=y bad-userdist.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: user-defined distributions are not allowed in --sstanc mode.

Reject protected sampling in control flow
  $ stanc --sstanc --sstan-protect=y bad-control-flow-observe.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: sampling statement for `y` appears inside control flow.

Reject non-data protected names
  $ stanc --sstanc --sstan-protect=not_data good.stan > /tmp/sstanc.err 2>&1 || true
  $ grep "SStan violation:" /tmp/sstanc.err
  SStan violation: --sstan-protect contains names not declared as top-level data variables: not_data.

Safe model compiles
  $ stanc --sstanc --sstan-protect=y good.stan

Multiple protected data variables compile
  $ stanc --sstanc --sstan-protect=x,y good-multi-protected.stan

Unprotected covariates can be used freely
  $ stanc --sstanc --sstan-protect=y good-unprotected-covariate.stan
