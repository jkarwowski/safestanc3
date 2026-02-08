// Compile with: stanc --sstanc --sstan-protect=y,z
data {
  real y;
  real z;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 1);
  y ~ normal(mu, 1);
  z ~ normal(y, 1);
}
