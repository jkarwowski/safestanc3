data {
  real y;
  real x;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  y ~ normal(theta * x, 1);
}
