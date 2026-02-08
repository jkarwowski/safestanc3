// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 1);
  sigma ~ exponential(1);

  y[1] ~ normal(mu, sigma);
}
