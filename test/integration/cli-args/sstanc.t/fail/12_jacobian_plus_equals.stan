// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
transformed parameters {
  jacobian += 1;
}
model {
  y ~ normal(0, 1);
}
