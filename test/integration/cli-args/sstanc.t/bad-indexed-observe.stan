data {
  int<lower=1> N;
  array[N] real y;
}
model {
  y[1] ~ normal(0, 1);
}
