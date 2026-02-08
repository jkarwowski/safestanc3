// Compile with: stanc --sstanc --sstan-protect=y
functions {
  real sneaky_lp(real x) {
    return x;
  }
}
data {
  real y;
}
parameters {
  real mu;
}
model {
  real z;

  mu ~ normal(0, 1);
  y ~ normal(mu, 1);

  z = sneaky_lp(0.0);
}
