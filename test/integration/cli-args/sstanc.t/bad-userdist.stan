functions {
  real custom_lpdf(real y, real mu) {
    return normal_lpdf(y | mu, 1);
  }
}
data {
  real y;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  y ~ custom(theta);
}
