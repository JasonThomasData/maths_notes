library(deSolve)

model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- mu*N - a*(B/(k+B))*S - mu*S
    dI <- a*(B/(k+B))*S - gamma*I - mu*I
    dR <- gamma*I - mu*R
    dB <- epsilon*I - alpha*B
    return(list(c(dS, dI, dR, dB)))
  })
}

diseaseFreePopulation = 100000
infectedAtStart = 1 
bacteriaAtStart = 0
init <- c(S=diseaseFreePopulation,
          I=infectedAtStart,
          R=0,
          B=bacteriaAtStart)
days <- 365*2
times <- seq(1,days,0.1)

alphaSpaceSize <- 20
epsilonSpaceSize <- 20

alphaSpace <- (1:alphaSpaceSize)/alphaSpaceSize
epsilonSpace <- (1:epsilonSpaceSize)/epsilonSpaceSize
maxInfected <- matrix(0, alphaSpaceSize, epsilonSpaceSize)
maxInfectedTime <- matrix(0, alphaSpaceSize, epsilonSpaceSize)
escapedInfection <- matrix(0, alphaSpaceSize, epsilonSpaceSize)

for (i in 1:alphaSpaceSize) {
  alpha <- alphaSpace[i]
  for (j in 1:epsilonSpaceSize) {
    epsilon <- epsilonSpace[j] 
    parameters <- c(N=diseaseFreePopulation,
                    mu=0.0001,
                    a=1,
                    k=10^6,
                    gamma=1/14,
                    epsilon=epsilon,
                    alpha=alpha)
    out <- ode(y=init, 
               times=times, 
               func=model, 
               parms=parameters, 
               method="ode45")
    
    # Day(time) is column 1, Infected is column 3
    indexOfMax <- which.max(out[,3])
    maxInfected[i,j] <- out[indexOfMax,3]
    maxInfectedTime[i,j] <- out[indexOfMax,1]
    escapedInfection[i,j] <- out[length(out[,2]),2]
  }
}

# plotly takes a matrix as x being columns, y being rows, generate a matrix via row multiplications to see it's true.

#install.packages("plotly")
library(plotly)

plot_ly(x = alphaSpace,
        y = epsilonSpace,
        z = maxInfected,
        type = "surface") %>%
  layout(title = 'Maximum number of people infected at on any single day during the outbreak',
         scene = list(xaxis = list(title = "epsilon"),
                      yaxis = list(title = "alpha"),
                      zaxis = list(title = "max infectious")))

plot_ly(x = alphaSpace,
        y = epsilonSpace,
        z = maxInfectedTime,
        type = "surface") %>%
  layout(title = 'Number of days since outbreak at peak number of infected on any single day',
         scene = list(xaxis = list(title = "epsilon"),
                      yaxis = list(title = "alpha"),
                      zaxis = list(title = "day")))

plot_ly(x = alphaSpace,
        y = epsilonSpace,
        z = escapedInfection,
        type = "surface") %>%
  layout(title = 'Number of people who have not been infected',
         scene = list(xaxis = list(title = "epsilon"),
                      yaxis = list(title = "alpha"),
                      zaxis = list(title = "people")))
