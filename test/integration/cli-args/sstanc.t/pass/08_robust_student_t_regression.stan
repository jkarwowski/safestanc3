// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> N;
  vector[N] y;
  vector[N] x;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
  real<lower=2> nu;
}
model {
  alpha ~ normal(0, 2);
  beta ~ normal(0, 2);
  sigma ~ exponential(1);
  nu ~ gamma(2, 0.1);

  y ~ student_t(nu, alpha + beta * x, sigma);
}
