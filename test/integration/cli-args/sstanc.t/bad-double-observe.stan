data {
  real y;
}
model {
  y ~ normal(0, 1);
  y ~ normal(1, 1);
}
