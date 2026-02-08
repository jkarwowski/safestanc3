// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  array[N] int<lower=0> y;
}
parameters {
  vector[K] beta;
}
model {
  beta ~ normal(0, 1);
  y ~ poisson_log(X * beta);
}
