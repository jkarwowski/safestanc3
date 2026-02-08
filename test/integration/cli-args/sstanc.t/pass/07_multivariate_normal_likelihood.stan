// Compile with: stanc --sstanc --sstan-protect=y
data {
  int<lower=1> D;
  vector[D] y;
  cov_matrix[D] Sigma;
}
parameters {
  vector[D] mu;
}
model {
  mu ~ normal(0, 1);
  y ~ multi_normal(mu, Sigma);
}
