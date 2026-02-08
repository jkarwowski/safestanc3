// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
transformed data {
  real y_copy;
  y_copy = y;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 1);
  y ~ normal(mu, 1);
}
