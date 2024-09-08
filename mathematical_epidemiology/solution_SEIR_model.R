# SEIR Epidemic in a Closed Population

library(deSolve)

SEIRmodel <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta*(S*I)/N
    dE <- beta*(S*I)/N -sigma*E
    dI <- sigma*E -gamma*I
    dR <- gamma*I
    return(list(c(dS, dE, dI, dR)))
  })
}

diseaseFreePopulation = 36000
infectedAtStart = 1
init <- c(S=diseaseFreePopulation, E=0, I=infectedAtStart, R=0)
times <- seq(1,365,0.1)

#### FAST MPOX

SEIRpars_fastMpox <- c(beta=2.4/14,
                       sigma=1,
                       gamma=1/14,
                       N=diseaseFreePopulation-infectedAtStart)

out_fastMpox <- ode(y=init, times=times, func=SEIRmodel, parms=SEIRpars_fastMpox, method="ode45")
out_fastMpox <- as.data.frame(out_fastMpox)

print(c("Total recovered population of fast mpox:", tail(out_fastMpox$R, n=1)))
maxInfection = max(out_fastMpox$I)
indexOfMaxInfection = match(maxInfection, out_fastMpox$I)
print(c("Max infectious on a single day:", maxInfection))
print(c("Day of peak infectious:", times[indexOfMaxInfection]))

plot(out_fastMpox$time, out_fastMpox$S, ylim = c(0, 36000), type='l', lty=1, col="red", lwd=2, xlab="Day of epidemic",
     ylab="", main="SEIR model - fast mpox")
lines(out_fastMpox$time, out_fastMpox$E, type='l', lty=2, col="blue", lwd=2, xlab="Day of epidemic",
      ylab="")
lines(out_fastMpox$time, out_fastMpox$I, type='l', lty=3, col="green", lwd=2, xlab="Day of epidemic",
      ylab="")
lines(out_fastMpox$time, out_fastMpox$R, type='l', lty=4, col="black", lwd=2, xlab="Day of epidemic",
      ylab="")
legend(190, 22000, legend=c("Susceptible", "Exposed", "Infectious", "Recovered"),
       col=c("red", "blue", "green", "black"), lty=1:4, cex=1.2)

#### SLOW MPOX

diseaseFreePopulation = 36000
infectedAtStart = 1
init <- c(S=diseaseFreePopulation, E=0, I=infectedAtStart, R=0)
times <- seq(1,6000,0.1)

SEIRpars_slowMpox <- c(beta=1.1/28,
                       sigma=1/7,
                       gamma=1/28,
                       N=diseaseFreePopulation-infectedAtStart)

out_slowMpox <- ode(y=init, times=times, func=SEIRmodel, parms=SEIRpars_slowMpox, method="ode45")
out_slowMpox <- as.data.frame(out_slowMpox)

print(c("Total recovered population slow mpox:", tail(out_slowMpox$R, n=1)))
maxInfection = max(out_slowMpox$I)
indexOfMaxInfection = match(maxInfection, out_slowMpox$I)
print(c("Max infectous on a single day:", maxInfection))
print(c("Day of peak infectious:", times[indexOfMaxInfection]))

plot(out_slowMpox$time, out_slowMpox$S, ylim = c(0, 36000), type='l', lty=1, col="red", lwd=2, xlab="Day of epidemic",
     ylab="", main="SEIR model - slow mpox")
lines(out_slowMpox$time, out_slowMpox$E, type='l', lty=2, col="blue", lwd=2, xlab="Day of epidemic",
      ylab="")
lines(out_slowMpox$time, out_slowMpox$I, type='l', lty=3, col="green", lwd=2, xlab="Day of epidemic",
      ylab="")
lines(out_slowMpox$time, out_slowMpox$R, type='l', lty=4, col="black", lwd=2, xlab="Day of epidemic",
      ylab="")
legend(190, 22000, legend=c("Susceptible", "Exposed", "Infectious", "Recovered"),
       col=c("red", "blue", "green", "black"), lty=1:4, cex=1.2)