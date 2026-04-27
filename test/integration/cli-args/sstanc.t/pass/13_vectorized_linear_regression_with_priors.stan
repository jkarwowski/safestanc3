// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real a;
  real b;
  real<lower=0> sigma;
}
model {
  a ~ normal(0, 5);
  b ~ normal(0, 5);
  sigma ~ exponential(1);
  y ~ normal(a + b * x, sigma);
}
