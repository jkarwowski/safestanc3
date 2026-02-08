// ARMA(1, 1)

data {
  int<lower=1> T; // number of observations
  array[T] real y_hist; // unprotected history used for lag recursion
  array[T] real y_obs; // protected observations
}
parameters {
  real mu; // mean coefficient
  real phi; // autoregression coefficient
  real theta; // moving average coefficient
  real<lower=0> sigma; // noise scale
}
model {
  vector[T] nu; // prediction for time t
  vector[T] err; // error for time t

  mu ~ normal(0, 10);
  phi ~ normal(0, 2);
  theta ~ normal(0, 2);
  sigma ~ cauchy(0, 2.5);

  nu[1] = mu + phi * mu; // assume err[0] == 0
  err[1] = y_hist[1] - nu[1];
  for (t in 2 : T) {
    nu[t] = mu + phi * y_hist[t - 1] + theta * err[t - 1];
    err[t] = y_hist[t] - nu[t];
  }

  y_obs ~ normal(nu, sigma);
}
