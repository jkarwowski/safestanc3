# SafeStan (SStan) for stanc3

This fork adds a SafeStan mode to `stanc3` for adversarial settings where the
model author is not trusted.

SafeStan is a static (compile-time) restriction layer over Stan. Its purpose is
to prevent likelihood hacking on a designated protected data interface.

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
   - Forbids `_lp` function definitions and calls

2. Protected data single-use:
   - Every protected variable must be a top-level `data` variable.
   - Each protected variable must appear exactly once on the left side of `~`.
   - Left side must be a plain identifier (no indexing/projections/expressions).
   - Protected variables cannot be used before they are observed.
   - Protected variables cannot be referenced in transformed data/parameters.

3. Proper distribution path only:
   - Sampling statements must resolve to built-in Stan distributions.
   - User-defined distributions are rejected in `~`.

4. Strict parameter discipline (enabled in this fork):
   - Each parameter must appear in exactly one sampling statement.
   - Parameters cannot be used before their sampling statement.

5. Control-flow safety:
   - Protected observations and parameter-prior sampling statements cannot appear
     inside `if`/`for`/`while`.

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

Broader CLI checks:

```bash
opam exec -- dune runtest test/integration/cli-args
```

## Current Scope and Limitations

- This implementation focuses on compile-time safety checks.
- Compiler-generated trusted log-likelihood output is not yet emitted.
- The rules are intentionally conservative and may reject some otherwise valid
  Stan programs.
