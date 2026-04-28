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
  if (phi > 0) {
    theta ~ normal(0, 1);
    y ~ normal(theta, 1);
  } else {
    theta ~ normal(0, 1);
    y ~ normal(theta, 1);
  }
}
