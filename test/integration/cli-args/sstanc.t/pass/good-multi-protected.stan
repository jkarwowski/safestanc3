data {
  real x;
  real y;
}
model {
  x ~ normal(0, 1);
  y ~ normal(x, 1);
}
