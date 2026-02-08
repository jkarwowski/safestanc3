data {
  int<lower=0, upper=1> y;
}
model {
  target += 1;
  y ~ bernoulli(0.5);
}
