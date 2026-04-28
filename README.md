# SafeStan (SStan) for stanc3

This fork adds a SafeStan mode to `stanc3` for adversarial settings where the
model author is not trusted.

SafeStan is a static (compile-time) restriction layer over Stan. Its purpose is
to prevent *likelihood hacking* on a designated protected data interface, as
described in [Likelihood hacking in probabilistic program
synthesis](https://arxiv.org/abs/2603.24126).

SafeStan is designed so that generated models report a normalized marginal
log-likelihood for the interface being modelled. To do this, it assigns roles to
top-level inputs and parameters. Variables named by `--sstan-protect` are
modelled interface variables: each must be scored by a built-in distribution,
exactly once, before any other use. They cannot be inspected in an `if`, used to
compute helper values, or referenced in transformed blocks before that scoring
statement. After a modelled interface variable has been scored, it is available
as an ordinary model-block quantity and may be used in later computations or
later likelihood terms. Other top-level `data` variables are context inputs:
they may be used freely as predictors, constants, indices, or configuration, but
they may not appear on the left side of a sampling statement. Parameters are
latent variables: each parameter must have exactly one built-in prior sampling
statement before it is used in the model block. SafeStan also rejects constructs
that can directly inflate, edit, or gate the log-likelihood, such as
`target +=`, `target()`, `jacobian +=`, `reject()`, `fatal_error()`, and `_lp`
functions.

The original upstream-style compiler README is preserved as `README_original`.

## Quick Start

Build:

```bash
opam exec -- dune build src/stanc/stanc.exe
```

Compile a model in SafeStan mode:

```bash
stanc --sstanc --sstan-protect=y,obs_x model.stan
```

`--sstan-protect` is required when `--sstanc` is enabled.

## What SafeStan Enforces

When `--sstanc` is on:

1. No arbitrary scoring:
   - Forbids `target += ...`
   - Forbids `target()`
   - Forbids `jacobian += ...`
   - Forbids `reject()` and `fatal_error()`
   - Forbids `_lp` function definitions and calls

2. Protected data single-use:
   - Every protected variable must be a top-level `data` variable.
   - Each protected variable must appear exactly once on the left side of `~`.
   - Left side must be a plain identifier (no indexing/projections/expressions).
   - Protected variables cannot be used before they are observed.
   - After observation, a protected variable is treated as a normal value.
   - Protected variables cannot be referenced in transformed data/parameters.

3. Proper distribution path only:
   - Sampling statements must resolve to built-in Stan distributions.
   - User-defined distributions are rejected in `~`.

4. Strict parameter discipline:
   - Each parameter must appear in exactly one sampling statement.
   - Parameters cannot be used in the model block before their sampling
     statement.

5. Control-flow safety:
   - Protected observations and parameter-prior sampling statements cannot appear
     inside loops (`for`/`while`/`foreach`).
   - Conditional (`if/else`) sampling is allowed only when both branches produce
     identical sampling effects for protected variables and parameters.

6. Reserved identifier:
   - `sstan_trusted_loglik__` is reserved in SafeStan mode.

If `--sstanc` is off, standard `stanc3` behavior is unchanged.

## Diagnostics

Violations are emitted as normal semantic errors with source locations and
messages prefixed with `SStan violation:`.

## Minimal Example

Accepted:

```stan
data { int<lower=0,upper=1> y; }
parameters { real<lower=0,upper=1> theta; }
model {
  theta ~ beta(1, 1);
  y ~ bernoulli(theta);
}
```

Rejected (arbitrary score edit):

```stan
data { int y; }
model {
  target += 1000;
  y ~ poisson(1);
}
```

## Implementation Notes

- CLI flags are wired in `src/stanc/CLI.ml`.
- SafeStan settings are carried in `src/driver/Flags.ml`.
- Checks are implemented in `src/frontend/Sstan_check.ml`.
- The pass runs post-typecheck in `src/driver/Entry.ml`.
- A dedicated error constructor was added in `src/frontend/Semantic_error.ml`.
- `stancjs` flag parsing was updated in `src/stancjs/stancjs.ml`.

## Tests

SafeStan integration coverage:

```bash
opam exec -- dune runtest test/integration/cli-args/sstanc.t
```

Benchmark-model comparison fixture:

- Location: `test/integration/cli-args/sstanc.t/compared`
- Contents: 10 paired models, each with:
  - `*.original.stan`: original Stan integration model
  - `*.safestan.stan`: SafeStan transplant (or an intentional failing variant
    when not transplantable)
- Harness: included in
  `test/integration/cli-args/sstanc.t/run.t` and exercised by the same
  `dune runtest` command above.
- Notes for each model are in
  `test/integration/cli-args/sstanc.t/compared/README.md`.

Broader CLI checks:

```bash
opam exec -- dune runtest test/integration/cli-args
```

## Current Scope and Limitations

- This implementation focuses on compile-time safety checks.
- Compiler-generated trusted log-likelihood output is not yet emitted.
- The rules are intentionally conservative and may reject some otherwise valid
  Stan programs.
