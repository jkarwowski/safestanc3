// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 1);
  sigma ~ exponential(1);

  if (mu > 0)
    y ~ normal(mu, sigma);
  else
    y ~ normal(mu, sigma);
}
