// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  beta ~ normal(0, 1);
  sigma ~ exponential(1);
  y ~ normal(X * beta, sigma);
}
