// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
parameters {
  real theta;
}
transformed parameters {
  real phi = theta;
}
model {
  y ~ normal(phi, 1);
  theta ~ normal(0, 1);
}
