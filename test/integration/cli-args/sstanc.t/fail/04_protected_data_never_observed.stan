// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
parameters {
  real mu;
}
model {
  mu ~ normal(0, 1);
}
