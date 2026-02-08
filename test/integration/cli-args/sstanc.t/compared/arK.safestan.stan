data {
  int<lower=0> K;
  int<lower=0> T;
  array[T] real y_hist;
  array[T - K] real y_obs;
}
parameters {
  real alpha;
  array[K] real beta;
  real<lower=0> sigma;
}
model {
  vector[T - K] mu;

  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 2.5);

  for (t in (K + 1) : T) {
    real mu_t;
    mu_t = alpha;

    for (k in 1 : K)
      mu_t = mu_t + beta[k] * y_hist[t - k];

    mu[t - K] = mu_t;
  }

  y_obs ~ normal(mu, sigma);
}
