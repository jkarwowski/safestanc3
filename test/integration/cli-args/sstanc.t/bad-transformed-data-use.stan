data {
  real y;
}
transformed data {
  real z = y;
}
model {
  y ~ normal(0, 1);
}
