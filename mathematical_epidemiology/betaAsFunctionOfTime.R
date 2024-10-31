heavySideStep = function(d) {
  if (d < 0) {
    (0)
  } else {
    (1)
  }
}

beta_t = function(t) {
  beta_0 = 0
  beta_1 = 1
  (beta_0 + beta_1*cos(2*pi*t))
}

beta_d = function(d) {
  beta_0 = 1
  beta_0*(heavySideStep(d - 220) - heavySideStep(d - 280))
}

x = seq(-5,5,0.01)
y = beta_d(x)
plot(x,y, type="l")