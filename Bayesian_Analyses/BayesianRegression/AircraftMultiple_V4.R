graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
library(ggplot2)
library(ggpubr)
library(ks)
library(rjags)
library(runjags)
source("DBDA2E-utilities.R")      

#===============PRELIMINARY FUNCTIONS FOR POSTERIOR INFERENCES====================

smryMCMC_HD = function(  codaSamples , compVal = NULL,  saveName=NULL) {
    summaryInfo = NULL
    mcmcMat = as.matrix(codaSamples,chains=TRUE)
    paramName = colnames(mcmcMat)
    for ( pName in paramName ) {
      if (pName %in% colnames(compVal)){
          if (!is.na(compVal[pName])) {
            summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] , 
                                                              compVal = as.numeric(compVal[pName]) ))
          }
          else {
            summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] ) )
          }
      } else {
        summaryInfo = rbind( summaryInfo , summarizePost( paramSampleVec = mcmcMat[,pName] ) )
      }
    }
    rownames(summaryInfo) = paramName
    
  # summaryInfo = rbind( summaryInfo , 
  #                      "tau" = summarizePost( mcmcMat[,"tau"] ) )
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  return( summaryInfo )
}

#===============================================================================

plotMCMC_HD = function( codaSamples , data , xName="x" , yName="y" ,
                     showCurve=FALSE ,  pairsPlot=FALSE , compVal = NULL,
                     saveName=NULL , saveType="jpg" ) {
  # showCurve is TRUE or FALSE and indicates whether the posterior should
  #   be displayed as a histogram (by default) or by an approximate curve.
  # pairsPlot is TRUE or FALSE and indicates whether scatterplots of pairs
  #   of parameters should be displayed.
  #-----------------------------------------------------------------------------
  y = data[,yName]
  x = as.matrix(data[,xName])
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  zbeta0 = mcmcMat[,"zbeta0"]
  zbeta  = mcmcMat[,grep("^zbeta$|^zbeta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { zbeta = matrix( zbeta , ncol=1 ) }
  zVar = mcmcMat[,"zVar"]
  beta0 = mcmcMat[,"beta0"]
  beta  = mcmcMat[,grep("^beta$|^beta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { beta = matrix( beta , ncol=1 ) }
  tau = mcmcMat[,"tau"]
  pred1 = mcmcMat[,"pred[1]"] # Added by Demirhan
  pred2 = mcmcMat[,"pred[2]"] # Added by Demirhan
  #-----------------------------------------------------------------------------
  # Compute R^2 for credible parameters:
  YcorX = cor( y , x ) # correlation of y with each x predictor
  Rsq = zbeta %*% matrix( YcorX , ncol=1 )
  #-----------------------------------------------------------------------------
  if ( pairsPlot ) {
    # Plot the parameters pairwise, to see correlations:
    openGraph()
    nPtToPlot = 1000
    plotIdx = floor(seq(1,chainLength,by=chainLength/nPtToPlot))
    panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...) {
      usr = par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt = format(c(r, 0.123456789), digits=digits)[1]
      txt = paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex=1.25 ) # was cex=cex.cor*r
    }
    pairs( cbind( beta0 , beta , tau )[plotIdx,] ,
           labels=c( "beta[0]" , 
                     paste0("beta[",1:ncol(beta),"]\n",xName) , 
                     expression(tau) ) , 
           lower.panel=panel.cor , col="skyblue" )
    if ( !is.null(saveName) ) {
      saveGraph( file=paste(saveName,"PostPairs",sep=""), type=saveType)
    }
  }
  #-----------------------------------------------------------------------------
  # Marginal histograms:
  
  decideOpenGraph = function( panelCount , saveName , finished=FALSE , 
                              nRow=2 , nCol=3 ) {
    # If finishing a set:
    if ( finished==TRUE ) {
      if ( !is.null(saveName) ) {
        saveGraph( file=paste0(saveName,ceiling((panelCount-1)/(nRow*nCol))), 
                   type=saveType)
      }
      panelCount = 1 # re-set panelCount
      return(panelCount)
    } else {
      # If this is first panel of a graph:
      if ( ( panelCount %% (nRow*nCol) ) == 1 ) {
        # If previous graph was open, save previous one:
        if ( panelCount>1 & !is.null(saveName) ) {
          saveGraph( file=paste0(saveName,(panelCount%/%(nRow*nCol))), 
                     type=saveType)
        }
        # Open new graph
        openGraph(width=nCol*7.0/3,height=nRow*2.0)
        layout( matrix( 1:(nRow*nCol) , nrow=nRow, byrow=TRUE ) )
        par( mar=c(4,4,2.5,0.5) , mgp=c(2.5,0.7,0) )
      }
      # Increment and return panel count:
      panelCount = panelCount+1
      return(panelCount)
    }
  }
  
  # Original scale:
  panelCount = 1
  if (!is.na(compVal["beta0"])){
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
    histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(beta[0]) , main="Intercept", compVal = as.numeric(compVal["beta0"] ))
  } else {  
    histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(beta[0]) , main="Intercept")
  }
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
     if (!is.na(compVal[paste0("beta[",bIdx,"]")])) {
      histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx],
                         compVal = as.numeric(compVal[paste0("beta[",bIdx,"]")]))
    } else{
      histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                           xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx])
    }
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( tau , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(tau) , main=paste("Scale") )
  # panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  # histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve ,
  #                      xlab=bquote(R^2) , main=paste("Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount ,  saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( pred1 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab="pred1" , main="Prediction 1" ) # Added by Demirhan
  panelCount = decideOpenGraph( panelCount ,  saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( pred2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab="pred2" , main="Prediction 2" ) # Added by Demirhan
  
  # Standardized scale:
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zbeta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*beta[0]) , main="Intercept" )
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
    histInfo = plotPost( zbeta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(z*beta[.(bIdx)]) , main=xName[bIdx] )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zVar , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*tau) , main=paste("Scale") )
  # panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  # histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve ,
  #                      xlab=bquote(R^2) , main=paste("Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMargZ") )
  
  #-----------------------------------------------------------------------------
}

#===============PRELIMINARY FUNCTIONS FOR POSTERIOR INFERENCES====================


myData <- read.csv("aircraft.csv")
head(myData)

# Scatter plots
p1 <- ggplot(myData, aes(x=X1, y=Y)) +
  geom_point() +
  xlab("Aspect ratio") +
  ylab("Cost")

p2 <- ggplot(myData, aes(x=X2, y=Y)) +
  geom_point() +
  xlab("Lift-to-drag ratio") +
  ylab("Cost")


p3 <- ggplot(myData, aes(x=X3, y=Y)) +
  geom_point() +
  xlab("Weight of plane") +
  ylab("Cost")


p4 <- ggplot(myData, aes(x=X4, y=Y)) +
  geom_point() +
  xlab("Maximal thrust ") +
  ylab("Cost")

figure <- ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
figure

# Histogram
hist(myData$Y, main= " Histogram of the dependent variable", xlab = "Cost")
# Kernel density estimation
plot(kde(myData$Y), xlab = "Cost") # with default settings


# THE DATA.
y = myData[,"Y"]
x = as.matrix(myData[,c("X1","X2","X3","X4")])

# Some more descriptives
cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
show( round(cor(x),3) )
cat("\n")

xPred = array(NA, dim = c(2,4))
xPred[1,] = c(1.5, 4.5, 37500, 39000)
xPred[2,] = c(4, 3.1, 14500, 11500)

# Specify the data in a list, for later shipment to JAGS:
dataList <- list(
  x = x ,
  y = y ,
  xPred = xPred ,
  Nx = dim(x)[2] ,
  Ntotal = dim(x)[1]
)

# First run without initials!
initsList <- list(
  zbeta0 = 2000,
  zbeta = c(100, 1, 1, 0.5),
  Var = 12000000
)

# WE WILL RUN THE MODEL WITH SCALING!

# THE MODEL.
modelString = "
# Standardize the data:
data {
  ysd <- sd(y)
  for ( i in 1:Ntotal ) {
    zy[i] <- y[i] / ysd
  }
  for ( j in 1:Nx ) {
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- x[i,j] / xsd[j]
    }
  }
}
# Specify the model for scaled data:
model {
  for ( i in 1:Ntotal ) {
    zy[i] ~ dgamma( (mu[i]^2)/zVar , mu[i]/zVar ) 
    mu[i] <- zbeta0 + sum( zbeta[1:Nx] * zx[i,1:Nx] ) 
  }
  # Priors on standardized scale:
  zbeta0 ~ dnorm( 0 , 1/2^2 )  # 1/ variance for normal distribution
  zbeta[1] ~ dnorm( 2/xsd[1] , 1/(4/xsd[1]^2) ) # 1/ variance for normal distribution
  zbeta[2] ~ dnorm( 0 , 1/4 ) # 1/ variance for normal distribution
  zbeta[3] ~ dnorm( (60/100000)/xsd[3] , 1/(1/xsd[3]^2) ) # 1/ variance for normal distribution
  zbeta[4] ~ dnorm( (30/100000)/xsd[4] , 1/(0.1/xsd[4]^2) ) # 1/ variance for normal distribution
  
  zVar ~ dgamma( 0.01 , 0.01 )
  # Transform to original scale:
  beta[1:Nx] <- ( zbeta[1:Nx] / xsd[1:Nx] ) * ysd
  beta0 <- zbeta0*ysd
  tau <- zVar * (ysd)^2

  # Compute predictions at every step of the MCMC
  for ( i in 1:2){
    pred[i] <- beta0 + beta[1] * xPred[i,1] + beta[2] * xPred[i,2] + beta[3] * xPred[i,3] + beta[4] * xPred[i,4]
  }
  # pred[1] <- beta0 + beta[1] * xPred[1,1] + beta[2] * xPred[1,2] + beta[3] * xPred[1,3] + beta[4] * xPred[1,4]
  # pred[2] <- beta0 + beta[1] * xPred[2,1] + beta[2] * xPred[2,2] + beta[3] * xPred[2,3] + beta[4] * xPred[2,4]

}
" # close quote for modelString
# Write out modelString to a text file
writeLines( modelString , con="TEMPmodel.txt" )


parameters = c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar") # Here beta is a vector!

adaptSteps = 500  # Number of steps to "tune" the samplers
burnInSteps = 1000
nChains = 2 
thinSteps = 3 # First run for 3
numSavedSteps = 10000
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Parallel run
runJagsOut <- run.jags( method="parallel" ,
                        model="TEMPmodel.txt" ,
                        monitor=c( "zbeta0" ,  "zbeta" , "beta0" ,  "beta" ,  "tau", "zVar", "pred")  ,
                        data=dataList ,
                        inits=initsList ,
                        n.chains=nChains ,
                        adapt=adaptSteps ,
                        burnin=burnInSteps ,
                        sample=numSavedSteps ,
                        thin=thinSteps , summarise=FALSE , plots=FALSE )
codaSamples = as.mcmc.list( runJagsOut )
save.image(file="multiReg_AircraftExample")

diagMCMC( codaSamples , parName="beta0" )
diagMCMC( codaSamples , parName="beta[1]" )
diagMCMC( codaSamples , parName="beta[2]" )
diagMCMC( codaSamples , parName="beta[3]" )
diagMCMC( codaSamples , parName="beta[4]" )
diagMCMC( codaSamples , parName="pred[1]" )
diagMCMC( codaSamples , parName="pred[2]" )


# save.image(file="1KBurnin_20KSavedIter_21Thin.RData")
# load(file="10KBurnin_200KSavedIter_2Thin.RData") 
# load(file="1KBurnin_20KSavedIter_21Thin.RData")
nrow(codaSamples[[1]])

# Do further thinning from the codaSamples
furtherThin <- 7
thiningSequence <- seq(1,nrow(codaSamples[[1]]), furtherThin)
newCodaSamples <- mcmc.list()
for ( i in 1:nChains){
  newCodaSamples[[i]] <- as.mcmc(codaSamples[[i]][thiningSequence,])
}

summary(codaSamples)

summary(newCodaSamples)
# Do further thinning from the codaSamples


# Finding predictions outside of MCMC
findPredsFor = c(1, 1.5, 4.5, 37500, 39000) # X-values you want to find predictions for
xPredAfterMCMC <- codaSamples[,1] # This is just to create xPredAfterMCMC with right dimentions to write new data into it
for ( i in 1:nChains){
  for (t in 1:1000){ # numSavedSteps
    xPredAfterMCMC[[i]][t] = sum(codaSamples[[ i ]][ t , 6:10] * findPredsFor) # apply y = X*beta model to find the predictions for each realisation in the generated chains
  }
}
codaSamplesPreds = as.mcmc.list(xPredAfterMCMC)
varnames(codaSamplesPreds) <- "NewPred"

diagMCMC( codaSamplesPreds , parName="NewPred" )
summaryInfo <- smryMCMC_HD( codaSamples = codaSamplesPreds  )
summaryInfo
# Finding predictions outside of MCMC

library(coda)
gelman.plot(codaSamples, confidence = 0.95, transform=FALSE, autoburnin=FALSE)

diagMCMC( codaSamples , parName="beta0" )
diagMCMC( codaSamples , parName="beta[1]" )
diagMCMC( codaSamples , parName="beta[2]" )
diagMCMC( codaSamples , parName="beta[3]" )
diagMCMC( codaSamples , parName="beta[4]" )
diagMCMC( codaSamples , parName="tau" )
diagMCMC( codaSamples , parName="pred[1]" )
diagMCMC( codaSamples , parName="pred[2]" )
diagMCMC( codaSamples , parName="zbeta0" )
diagMCMC( codaSamples , parName="zbeta[1]" )
diagMCMC( codaSamples , parName="zbeta[2]" )
diagMCMC( codaSamples , parName="zbeta[3]" )
diagMCMC( codaSamples , parName="zbeta[4]" )
graphics.off()

compVal <- data.frame("beta0" = NA, "beta[1]" = NA, "beta[2]" = NA, "beta[3]" = NA, "beta[4]" =  NA, "tau" = NA , check.names=FALSE)

summaryInfo <- smryMCMC_HD( codaSamples = codaSamples , compVal = compVal )
print(summaryInfo)

plotMCMC_HD( codaSamples = codaSamples , data = myData, xName=c("X1","X2","X3","X4") , 
          yName="Y", compVal = compVal)


# ============ Predictive check ============
coefficients <- summaryInfo[7:11,3] # Get the model coefficients out
Variance <- summaryInfo[12,3] # Get the variance out
# Since we imposed the regression model on the mean of the gamma likelihood,
# we use the model (X*beta) to generate the mean of gamma population for each 
# observed x vector. 
meanGamma <- as.matrix(cbind(rep(1,nrow(x)),  x)) %*% as.vector(coefficients)
# Generate random data from the posterior distribution. Here I take the 
# reparameterisation back to alpha and beta.
randomData <- rgamma(n= 231,shape=meanGamma^2/Variance, rate = meanGamma/Variance)

# Display the density plot of observed data and posterior distribution:
predicted <- data.frame(elapsed = randomData)
observed <- data.frame(elapsed = y)
predicted$type <- "Predicted"
observed$type <- "Observed"
dataPred <- rbind(predicted, observed)

ggplot(dataPred, aes(elapsed, fill = type)) + geom_density(alpha = 0.2)
