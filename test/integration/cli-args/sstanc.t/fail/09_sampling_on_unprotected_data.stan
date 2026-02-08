// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
  real x;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 1);

  x ~ normal(0, 1);
  y ~ normal(mu, 1);
}
