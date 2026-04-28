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
  theta ~ normal(0, 1);
  y ~ normal(phi, 1);
}
