data {
  real y;
}
model {
  real mu = y;
  y ~ normal(0, 1);
}
