// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 2);
  sigma ~ exponential(1);
  y ~ normal(mu, sigma);
}
