// Compile with: stanc --sstanc --sstan-protect=y
functions {
  real softplus(real x) {
    return log1p_exp(x);
  }
}
data {
  real y;
  real x;
}
parameters {
  real alpha;
  real beta;
  real sigma_raw;
}
transformed parameters {
  real<lower=0> sigma;
  sigma = softplus(sigma_raw);
}
model {
  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma_raw ~ normal(0, 1);

  y ~ normal(alpha + beta * x, sigma);
}
generated quantities {
  real y_hat;
  y_hat = alpha + beta * x;
}
