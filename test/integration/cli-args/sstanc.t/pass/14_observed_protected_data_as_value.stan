// Compile with: stanc --sstanc --sstan-protect=y,z
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  vector[N] z;
}
parameters {
  real a;
  real b;
  real<lower=0> sigma;
}
model {
  y ~ normal(a + b * x, sigma);
  z ~ normal(y, sigma);
}
