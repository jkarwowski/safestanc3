// Compile with: stanc --sstanc --sstan-protect=y
functions {
  real mynorm_lpdf(real x, real mu, real sigma) {
    return normal_lpdf(x | mu, sigma);
  }
}
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

  y ~ mynorm(mu, sigma);
}
