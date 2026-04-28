// Compile with: stanc --sstanc --sstan-protect=y
data {
  real y;
}
model {
  y ~ normal(0, 1);
  fatal_error("manual support edit");
}
