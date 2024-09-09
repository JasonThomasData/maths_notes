graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
library(ggplot2)
library(ggpubr)
library(ks)
library(rjags)
library(runjags)
source("DBDA2E-utilities.R")
source("providedCode.R")

myData <- read.csv("Assignment2PropertyPricesSmaller3.csv")
#myData$Type <- as.factor(myData$PropertyType)
# Above, I think that since property type is already numeric then it can stay as-is
head(myData)

# THE DATA.
y = myData[,"SalePrice.100K."]
x = as.matrix(myData[,c("Area","Bedrooms","Bathrooms","CarParks","PropertyType")])

cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
show( round(cor(x),3) )
cat("\n")

hist(myData$Area)
hist(myData$Bedrooms)
hist(myData$Bathrooms)
hist(myData$CarParks)

dataList <- list(
  x = x ,
  y = y ,
  Nx = dim(x)[2] ,    # variable count
  Ntotal = dim(x)[1]  # observation count
)

# JAGS failed without initial values, so these were set:
initsList <- list(
  zbeta0 = 1000,
  zbeta = c(10,10,10,10,10),
  var = 12000000
)
# But it seems that with enough adaptation steps, we don't need it.

# allow jags to determine initialising values

# THE MODEL.
modelString = "
# Standardize the data:
data {
  priorInfo[1] <- 90
  priorInfo[2] <- 100000
  priorInfo[3] <- 120000
  priorInfo[4] <- 0
  priorInfo[5] <- -150000 # unit = 1, house = 0, unit sells $150000 less

  ysd <- sd(y)
  for ( i in 1:Ntotal ) {
    zy[i] <- y[i] / ysd
  }
  for ( j in 1:Nx ) {
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- ifelse( j == Nx , x[i,j] , x[i,j] / xsd[j] ) # last var is dummy, don't standardise it
      # zx[i,j] <- x[i,j] / xsd[j]
    }
    zpriorInfo[j] <- ifelse( j == Nx , priorInfo[j] , priorInfo[j] / xsd[j] )
    # zpriorInfo[j] <- priorInfo[j] / xsd[j]
  }

  # Specify the values of indepdenent variables for prediction
  xPred[1,1] <- 600
  xPred[2,1] <- 800
  xPred[3,1] <- 1500
  xPred[4,1] <- 2500
  xPred[5,1] <- 250
  xPred[1,2] <- 2
  xPred[2,2] <- 3
  xPred[3,2] <- 2
  xPred[4,2] <- 5
  xPred[5,2] <- 3
  xPred[1,3] <- 2
  xPred[2,3] <- 1
  xPred[3,3] <- 1
  xPred[4,3] <- 4
  xPred[5,3] <- 2
  xPred[1,4] <- 1
  xPred[2,4] <- 2
  xPred[3,4] <- 1
  xPred[4,4] <- 4
  xPred[5,4] <- 1
  xPred[1,5] <- 1 # unit
  xPred[2,5] <- 0
  xPred[3,5] <- 0
  xPred[4,5] <- 0
  xPred[5,5] <- 1 # unit
}
# Specify the model for scaled data:
model {
  for ( i in 1:Ntotal ) {
    zy[i] ~ dgamma( (mu[i]^2)/zVar , mu[i]/zVar ) 
    mu[i] <- zbeta0 + sum( zbeta[1:Nx] * zx[i,1:Nx] ) 
  }

  # intercept prior - Assume prior weight is average, which should be 1 in std norm
  zbeta0 ~ dnorm( 0 , 1 ) 
  
  # other betas - since these are standard, then sd=1 would be standard.
  zbeta[1] ~ dnorm(zpriorInfo[1], 1/4)
  zbeta[2] ~ dnorm(zpriorInfo[2], 2)
  zbeta[3] ~ dnorm(zpriorInfo[3], 6)
  zbeta[4] ~ dnorm(zpriorInfo[4], 1/2)
  zbeta[5] ~ dnorm(zpriorInfo[5], 1/4) 
  
  # prior for sigma^2, used for the final gamma
  zVar ~ dgamma( 0.01 , 0.00100 )
  # Transform to original scale:
  
  beta[1:(Nx-1)] <- ( zbeta[1:(Nx-1)] / xsd[1:(Nx-1)] ) * ysd
  beta[Nx] <- zbeta[Nx] * ysd
  # beta[1:Nx] <- ( zbeta[1:Nx] / xsd[1:Nx] ) * ysd
  beta0 <- zbeta0*ysd
  tau <- zVar * (ysd)^2

  # Compute predictions at every step of the MCMC
  
  for ( i in 1:5 ) {
    pred[i] <- beta0 + beta[1] * xPred[i,1] + beta[2] * xPred[i,2] + beta[3] * xPred[i,3] + beta[4] * xPred[i,4] + beta[5] * xPred[i,5]
  }
}
" # close quote for modelString
# Write out modelString to a text file
writeLines( modelString , con="TEMPmodel.txt" )



adaptSteps = 4000  # Number of steps to "tune" the samplers
burnInSteps = 4000
nChains = 4
thinSteps = 10
numSavedSteps = 5000
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )


# Parallel run - instead of jags.model and burn-in
startTime = proc.time()
runJagsOut <- run.jags( method="parallel" ,
                        model="TEMPmodel.txt" ,
                        monitor=c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar", "pred")  ,
                        data=dataList ,
                        #inits=initsList ,
                        n.chains=nChains ,
                        adapt=adaptSteps ,
                        burnin=burnInSteps ,
                        sample=numSavedSteps ,
                        thin=thinSteps , summarise=FALSE , plots=FALSE )
codaSamples = as.mcmc.list( runJagsOut )
stopTime = proc.time()
elapsedTime = stopTime - startTime
show(elapsedTime)

# Run with adaptSteps = 500; burnInSteps = 10000; nChains = 2; thinSteps = 23; numSavedSteps = 5000; 115000 iterations
# Elapsed time: 597.725 sec
save.image(file='MultRegChainsR2.RData')
# load('MultRegChainsR2.RData')

diagMCMC( codaSamples , parName="zbeta0" )
diagMCMC( codaSamples , parName="zbeta[1]" )
diagMCMC( codaSamples , parName="zbeta[2]" )
diagMCMC( codaSamples , parName="zbeta[3]" )
diagMCMC( codaSamples , parName="zbeta[4]" )
diagMCMC( codaSamples , parName="zbeta[5]" )
diagMCMC( codaSamples , parName="tau" )