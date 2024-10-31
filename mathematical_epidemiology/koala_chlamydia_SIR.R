library("deSolve")
library("reshape2")
library("ggplot2")

# CODE FOR ASSIGNMENT 3 RE KOALA CHLAMYDIA
  
multiHostModel <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS_f <- r*(S_f + alpha*I_f) - r*N*S_f - beta_fm*(S_f*I_m)/N
    dI_f <- beta_fm*(S_f*I_m)/N - r*N*I_f - mu*I_f
    dS_m <- r*(S_f + alpha*I_f) - r*N*S_m - beta_mf*(S_m*I_f)/N
    dI_m <- beta_mf*(S_m*I_f)/N - r*N*I_m - mu*I_m
    dN <- 2*r*(S_f + alpha*I_f) - r*N^2 - mu*(I_f + I_m) # derived from dN = d(S_f + I_f + S_m + I_m)
    return(list(c(dS_f, dI_f, dS_m, dI_m, dN)))
  })
}
  
diseaseFreePopulation = 1
diseaseFreeMalePopulation = diseaseFreePopulation/2 # sex ratio 1:1
diseaseFreeFemalePopulation = diseaseFreePopulation/2
infectedMalesAtStart = .01 #Proportion of population
infectedFemalesAtStart = .01
init <- c(S_f=diseaseFreeFemalePopulation-infectedFemalesAtStart,
          I_f=infectedFemalesAtStart,
          S_m=diseaseFreeMalePopulation-infectedMalesAtStart,
          I_m=infectedMalesAtStart,
          N=diseaseFreePopulation)
times <- seq(1,150,0.1)
  
allData <- data.frame(matrix(ncol=8, nrow=0))
colnames(allData) <- c("time", "S_f", "S_m", "I_f", "I_m", "N", "alpha", "mu")
  
alphaValues <- c(0.3, 0.5, 0.7, 0.9)
muValues <- c(0.1, 0.2, 0.3, 0.4)
for( i in 1:length(alphaValues)) {
  alpha <- alphaValues[i]
  for( j in 1:length(muValues)) {
    mu <- muValues[j]

    # SOLVE
    multiHostPars <- c(r=0.2,
                       alpha=alpha,
                       beta_fm=1.2,
                       beta_mf=1,
                       mu=mu)
    out <- ode(y=init, times=times, func=multiHostModel, parms=multiHostPars, method="ode45")
    out <- as.data.frame(out)
    
    #CONCATENATE INTO ONE DF
    newData <- data.frame(time=out$time)
    newData[,"S_f"] <- out$S_f
    newData[,"S_m"] <- out$S_m
    newData[,"I_f"] <- out$I_f
    newData[,"I_m"] <- out$I_m
    newData[,"N"] <- out$N
    newData[,"alpha"] <- rep(alpha, length(times))
    newData[,"mu"] <- rep(mu, length(times))
    allData <- rbind(allData, newData)
  }
}
  
longData <- melt(allData, id.var=c("alpha", "mu", "time"))
names(longData) <- c("alpha", "mu", "day", "variable", "proportion")
  
ggplot(longData, aes(x=day, y=proportion)) + 
  geom_line(aes(color=variable)) +
  facet_grid(
    alpha ~ mu,
    labeller = labeller(
      alpha = c(`0.3` = "alpha = 0.3",
                `0.5` = "alpha = 0.5",
                `0.7` = "alpha = 0.7",
                `0.9` = "alpha = 0.9"),
      mu = c(`0.1` = "mu = 0.1",
             `0.2` = "mu = 0.2",
             `0.3` = "mu = 0.3",
             `0.4` = "mu = 0.4")
    )) +
  labs(
    title=expression(paste("For chlamydia affecting koalas with beta_mf = 1, beta_fm = 1.2, r = 0.2")),
    subtitle = sprintf("Assuming that initially %.2f of males and %.2f of females are infected", infectedMalesAtStart, infectedFemalesAtStart) 
  )
