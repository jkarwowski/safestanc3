// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> N;
  int<lower=1> G;
  array[N] int<lower=1, upper=G> g;
  vector[N] y;
}
parameters {
  vector[G] mu;
  real<lower=0> sigma;
}
model {
  vector[N] mu_g;

  mu ~ normal(0, 1);
  sigma ~ exponential(1);

  for (n in 1:N) {
    mu_g[n] = mu[g[n]];
  }

  y ~ normal(mu_g, sigma);
}
