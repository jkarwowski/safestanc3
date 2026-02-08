// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
parameters {
  real mu;
}
model {
  real m;

  mu ~ normal(0, 1);

  m = y;
  y ~ normal(mu, 1);
}
