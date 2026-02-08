functions {
  real helper_lp(real x) {
    return x;
  }
}
data {
  int<lower=0, upper=1> y;
}
model {
  y ~ bernoulli(0.5);
}
