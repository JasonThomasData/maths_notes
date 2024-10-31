betaContinuousCyclic <- function(t) {
  beta_0 = 1; #season
  beta_1 = -1; #season in middle of year
  y = beta_0 + beta_1*cos((2/365) * pi*t);  
}

heavySideStep <- function(d) {
  if (d < 0) {
    (0)
  } else {
    (1)
  }
}

betaDiscrete <- function(t) {
  beta_0 = 1; # season
  d = t %% 365;
  seasonStartDay = 365/4;
  seasonFinishDay = 3*365/4;
  y = beta_0 * (heavySideStep(d-seasonStartDay) - heavySideStep(d-seasonFinishDay));
}

numberOfYears = 1;
x = 0:(numberOfYears*365);

# Continuous plot
yContinuous = betaContinuousCyclic(x)

# Discrete plot
yDiscrete = c()
for (i in 1:length(x)) {
  yDiscrete = c(yDiscrete, betaDiscrete(x[i]))
}

plot(x,yContinuous, type="l");
lines(x,yDiscrete, type="l");

# And now, an SEIR model with seasonality

library(deSolve)

SEI_seasonal <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), 
    {
      beta <- beta_0 + beta_1*cos(2*pi*time/365)
      dS <- B - beta*S*(I/(S+E+I+R)) - mu*S
      dE <- beta*S*(I/(S+E+I+R)) - mu*E - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I - mu*R
      return(list(c(dS, dE, dI, dR)))
    }
  )
}

B <- 50
mu <- 1/(5*365)
beta_0 <- 1
beta_1 <- 0.5
sigma <- 1/7
gamma <- 1/7

SEI_seasonal_pars <- c(B=B,
                       mu=mu,
                       beta_0=beta_0,
                       beta_1=beta_1,
                       sigma=sigma,
                       gamma=gamma)

init <- c(S=(B/mu),
          E=1,
          I=0,
          R=0)

R0 <- (sigma/(sigma+mu))*(beta_0/(gamma+mu))
print(R0)

times <- seq(1, 10000, 0.1)
out <- ode(y=init, times=times, func=SEI_seasonal, parms=SEI_seasonal_pars, method="ode23")
out <- as.data.frame(out)
head(out)

plot(out$time, out$S, type="l", lty=1, col="red", lwd=2, xlab="Day of epidemic",ylab="", main="SIR model with seasonality")
lines(out$time, out$E, type="l", lty=1, col="blue", lwd=2, xlab="Day of epidemic",ylab="")
lines(out$time, out$I, type="l", lty=1, col="green", lwd=2, xlab="Day of epidemic",ylab="")