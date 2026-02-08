// Compile with: stanc --sstanc --sstan-protect=x_obs,y_obs
data {
  int<lower=1> N;
  vector[N] x_obs;
  vector[N] y_obs;
}
parameters {
  vector[N] x_true;
  real beta;
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
}
model {
  x_true ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma_x ~ exponential(1);
  sigma_y ~ exponential(1);

  x_obs ~ normal(x_true, sigma_x);
  y_obs ~ normal(beta * x_true, sigma_y);
}
