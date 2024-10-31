graphics.off()
rm(list=ls(all=TRUE))
library(ggplot2)
library(ggpubr)
library(ks)
library(rjags)
library(runjags)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("DBDA2E-utilities.R")

fileNameRoot="Appl2-" # for output filenames
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

plotMCMC_HD = function( codaSamples , data , xName="x" , yName="y", preds = FALSE ,
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
  # zbeta0 = mcmcMat[,"zbeta0"]
  # zbeta  = mcmcMat[,grep("^zbeta$|^zbeta\\[",colnames(mcmcMat))]
  # if ( ncol(x)==1 ) { zbeta = matrix( zbeta , ncol=1 ) }
  beta0 = mcmcMat[,"beta0"]
  beta  = mcmcMat[,grep("^beta$|^beta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { beta = matrix( beta , ncol=1 ) }
  if (preds){
    pred = mcmcMat[,grep("^pred$|^pred\\[",colnames(mcmcMat))]
  } # Added by Demirhan
  guess = mcmcMat[,"guess"]
  #-----------------------------------------------------------------------------
  # Compute R^2 for credible parameters:
  YcorX = cor( y , x ) # correlation of y with each x predictor
  Rsq = beta %*% matrix( YcorX , ncol=1 )
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
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( beta0 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(beta[0]) , main="Intercept", compVal = as.numeric(compVal["beta0"] ))
  for ( bIdx in 1:ncol(beta) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
    if (!is.na(compVal[paste0("beta[",bIdx,"]")])){
      histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                           xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx],
                           compVal = as.numeric(compVal[paste0("beta[",bIdx,"]")]))
    } else{
      histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                           xlab=bquote(beta[.(bIdx)]) , main=xName[bIdx])
    }
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(R^2) , main=paste("Prop Var Accntd") , finished=FALSE )
  
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( guess , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab="Guess parameter" , main=paste("Prop Var Accntd") , finished=TRUE )
  
  panelCount = 1
  if ( pred){
    
    for ( pIdx in 1:ncol(pred) ) {
      panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
      histInfo = plotPost( pred[,pIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                           xlab=bquote(pred[.(pIdx)]) , main=paste0("Prediction ",pIdx) ) 
    }
  }# Added by Demirhan
  # Standardized scale:
  panelCount = 1
  # panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  # histInfo = plotPost( zbeta0 , cex.lab = 1.75 , showCurve=showCurve ,
  #                      xlab=bquote(z*beta[0]) , main="Intercept" )
  # for ( bIdx in 1:ncol(beta) ) {
  #   panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  #   histInfo = plotPost( zbeta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
  #                        xlab=bquote(z*beta[.(bIdx)]) , main=xName[bIdx] )
  # }
  # panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  # histInfo = plotPost( Rsq , cex.lab = 1.75 , showCurve=showCurve ,
  #                      xlab=bquote(R^2) , main=paste("Prop Var Accntd") )
  # panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMargZ") )
  
  #-----------------------------------------------------------------------------
}

#===============PRELIMINARY FUNCTIONS FOR POSTERIOR INFERENCES====================

data <- read.csv("molecular_activity.csv")
head(data)
fac.activity <- as.factor(data$Activity)
table(fac.activity)
summary(data)

# Scatter plots
p1 <- ggplot(data, aes(x=prop_1, y=Activity)) +
  geom_point() +
  xlab("prop_1") +
  ylab("Activity")

p2 <- ggplot(data, aes(x=prop_2, y=Activity)) +
  geom_point() +
  xlab("prop_2") +
  ylab("Activity")

p3 <- ggplot(data, aes(x=prop_3, y=Activity)) +
  geom_point() +
  xlab("prop_3") +
  ylab("Activity")

p4 <- ggplot(data, aes(x=prop_4, y=Activity)) +
  geom_point() +
  xlab("prop_4") +
  ylab("Activity")

figure <- ggarrange(p1, p2 , p3, p4, nrow = 2, ncol = 2)
figure

# Histogram
hist(data$Activity, main= " Histogram of the dependent variable", xlab = "Activity")

summary(data[,-5])

#------------------------------------------------------------------------------

#------------------LOGISTIC REGRESSION WITHOUT MODEL UNCERTAINTY-------------------------
# THE DATA.
y = data[,"Activity"]
x = as.matrix(data[,c("prop_1","prop_2", "prop_3", "prop_4")])

cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
show( round(cor(x),3) )
cat("\n")
# prop_3 and prop_4 are highly correlated

x = as.matrix(data[,c("prop_1","prop_2", "prop_3")])

cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
show( round(cor(x),3) )
cat("\n")

# Load data
dataList <- list(
  x = x ,
  y = y ,
  Nx = dim(x)[2] ,
  Ntotal = dim(x)[1]
)

initsList <- list(
  zbeta0 = 0.001,
  zbeta = c(.0001, 2, 6),
  zVar = 1
)


modelString = "
data {
  for ( j in 1:Nx ) {
    xm[j]  <- mean(x[,j])
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
    }
  }
}
model {
  for ( i in 1:Ntotal ) {
    # In JAGS, ilogit is logistic:
    y[i] ~ dbern( mu[i] )
      mu[i] <- ( guess*(1/2) + (1.0-guess)*ilogit(zbeta0+sum(zbeta[1:Nx]*zx[i,1:Nx])) )
  }
  # Priors vague on standardized scale:
  zbeta0 ~ dnorm( 0 , 1/2^2 )
  # non-informative run
  for ( j in 1:Nx ) {
    zbeta[j] ~ dnorm( 0 , 1/2^2 )
  }
  guess ~ dbeta(1,9)
  # Transform to original scale:
  beta[1:Nx] <- zbeta[1:Nx] / xsd[1:Nx]
  beta0 <- zbeta0 - sum( zbeta[1:Nx] * xm[1:Nx] / xsd[1:Nx] )
}
"
writeLines( modelString , con="TEMPmodel.txt" )

parameters = c( "beta0" ,  "beta", "zbeta0", "zbeta", "guess") 

adaptSteps = 1000  # Number of steps to "tune" the samplers
burnInSteps = 1000
nChains = 2 
thinSteps = 3#31 # First run for 3
numSavedSteps = 2000
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Parallel run
startTime = proc.time()
runJagsOut <- run.jags( method="parallel" ,
                        model="TEMPmodel.txt" ,
                        monitor=parameters  ,
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

# save.image("REvironmentLojReg1Model.RData")
# adaptSteps = 1000; burnInSteps = 1000
# nChains = 2; thinSteps =31
# numSavedSteps = 2000
# load("REvironmentLojReg1Model.RData")

parameterNames = varnames(codaSamples) # get all parameter names
for ( parName in parameterNames[5:9] ) {
  diagMCMC( codaSamples , parName=parName ,
            saveName=fileNameRoot , saveType="eps" )
}

graphics.off()

compVal <- data.frame("beta0" = 0, "beta[1]" = 0, "beta[2]" = 0 , "beta[3]" = 0, check.names=FALSE)

summaryInfo <- smryMCMC_HD( codaSamples = codaSamples , compVal = compVal )
print(summaryInfo)

plotMCMC_HD( codaSamples = codaSamples , data = data, xName=c("prop_1","prop_2", "prop_3") , 
             yName="Activity", compVal = compVal)

#------------------LOGISTIC REGRESSION WITHOUT MODEL UNCERTAINTY-------------------------

#------------------LOGISTIC REGRESSION 2 MODELS -------------------------
modelString = "
data {
  for ( j in 1:Nx ) {
    xm[j]  <- mean(x[,j])
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
    }
  }
}
model {
  for ( i in 1:Ntotal ) {
    # In JAGS, ilogit is logistic:
    y[i] ~ dbern( mu[i] )
      mu[i] <- ifelse(m == 1, (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[1]*zx[i,1]+zbeta[2]*zx[i,2]+zbeta[3]*zx[i,3])),
                              (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[1]*zx[i,1]+zbeta[2]*zx[i,2])) )
  }
  # Priors vague on standardized scale:
  zbeta0 ~ dnorm( 0 , 1/2^2 )
  # non-informative run
  for ( j in 1:Nx ) {
    zbeta[j] ~ dnorm( 0 , 1/2^2 )
  }
  guess ~ dbeta(1,9)
  # Prior for model
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .5
  mPriorProb[2] <- .5
  # Transform to original scale:
  beta[1:Nx] <- zbeta[1:Nx] / xsd[1:Nx]
  beta0 <- zbeta0 - sum( zbeta[1:Nx] * xm[1:Nx] / xsd[1:Nx] )
}
"
writeLines( modelString , con="TEMPmodel.txt" )

parameters = c( "beta0" ,  "beta", "zbeta0", "zbeta", "guess", "m")

adaptSteps = 50  # Number of steps to "tune" the samplers
burnInSteps = 5000
nChains = 2 
thinSteps = 3#37 # First run for 3
numSavedSteps = 2000
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Parallel run
startTime = proc.time()
runJagsOut <- run.jags( method="parallel" ,
                        model="TEMPmodel.txt" ,
                        monitor=parameters  ,
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

#    user  system elapsed 
#    1.307   0.661  68.912 

# save.image("REvironmentLojReg2Models.RData")
# adaptSteps = 5000; burnInSteps = 5000
# nChains = 2; thinSteps = 37
# numSavedSteps = 2000
# load("REvironmentLojReg2Models.RData")

parameterNames = varnames(codaSamples) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaSamples , parName=parName ,
            saveName=fileNameRoot , saveType="eps" )
}

graphics.off()

# Convert coda-object codaSamples to matrix object for easier handling.
mcmcMat = as.matrix( codaSamples , chains=TRUE )
m = mcmcMat[,"m"]
beta0 = mcmcMat[,"beta0"]
beta1 = mcmcMat[,"beta[1]"]
beta2 = mcmcMat[,"beta[2]"]
beta3 = mcmcMat[,"beta[3]"]

# Compute the proportion of m at each index value:
pM1 = sum( m == 1 ) / length( m )
pM2 = 1 - pM1
cat("P(m = 1|D) = ", pM1, "\nP(m = 2|D) = ", pM2, "\n")

# Extract theta values for each model index:
beta1M1 = beta1[ m == 1 ]
beta2M1 = beta2[ m == 1 ]
beta3M1 = beta3[ m == 1 ]

beta1M2 = beta1[ m == 2 ]
beta2M2 = beta2[ m == 2 ]
beta3M2 = beta3[ m == 2 ]

openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M1 , 
          main=bquote( beta1*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M1 , 
          main=bquote( beta2*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M1 , 
          main=bquote( beta3*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )

openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M2 , 
          main=bquote( beta1*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M2 , 
          main=bquote( beta2*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M2 , 
          main=bquote( beta3*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )


#------------------LOGISTIC REGRESSION 2 MODELS -------------------------


#------------------LOGISTIC REGRESSION 4 MODELS -------------------------


graphics.off()

set.seed(123121)
data <- read.csv("molecular_activity.csv")
trIdx <- 1:378#sample(1:539, 500, replace = FALSE)
testIdx <- setdiff(1:539, trIdx)
dataTrain <- data[trIdx,]
dataTest <- data[testIdx,]
  
y = dataTrain[,"Activity"]
x = as.matrix(dataTrain[,c("prop_1","prop_2", "prop_3")])

xPred = as.matrix(dataTest[,1:3])

# Specify the data in a list, for later shipment to JAGS:
dataList <- list(
  x = x ,
  y = y ,
  xPred = xPred ,
  Nx = dim(x)[2] ,
  Ntotal = dim(x)[1],
  Npred = dim(xPred)[1]
)

modelString = "
data {
  for ( j in 1:Nx ) {
    xm[j]  <- mean(x[,j])
    xsd[j] <-   sd(x[,j])
    for ( i in 1:Ntotal ) {
      zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
    }
  }
}
model {
  for ( i in 1:Ntotal ) {
    # In JAGS, ilogit is logistic:
    y[i] ~ dbern( mu[i] )
      mu[i] <- ifelse(m == 1, (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[1]*zx[i,1]+zbeta[2]*zx[i,2]+zbeta[3]*zx[i,3])),
                              ifelse( m == 2, (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[1]*zx[i,1]+zbeta[2]*zx[i,2])),
                              ifelse( m == 3, (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[1]*zx[i,1]+zbeta[3]*zx[i,3])),
                                              (guess*(1/2) + (1.0-guess)*ilogit(zbeta0+zbeta[2]*zx[i,2]+zbeta[3]*zx[i,3])))))
  }
  # Priors vague on standardized scale:
  zbeta0 ~ dnorm( 0 , 1/2^2 )
  # non-informative run
  for ( j in 1:Nx ) {
    zbeta[j] ~ dnorm( 0 , 1/2^2 )
  }
  guess ~ dbeta(1,9)
  # Prior for model
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .25
  mPriorProb[2] <- .25
  mPriorProb[3] <- .25
  mPriorProb[4] <- .25
  # Transform to original scale:
  beta[1:Nx] <- zbeta[1:Nx] / xsd[1:Nx]
  beta0 <- zbeta0 - sum( zbeta[1:Nx] * xm[1:Nx] / xsd[1:Nx] )
  # Predictions
  for ( k in 1:Npred){
    pred[k] <- ilogit(beta0 + sum(beta[1:Nx] * xPred[k,1:Nx]))
  }
}
"
writeLines( modelString , con="TEMPmodel.txt" )

parameters = c( "beta0" ,  "beta", "m", "pred") # Here beta is a vector!

adaptSteps = 500  # Number of steps to "tune" the samplers
burnInSteps = 500
nChains = 2 
thinSteps = 2 #31 # First run for 3
numSavedSteps = 1000
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )

# Parallel run
startTime = proc.time()
runJagsOut <- run.jags( method="parallel" ,
                        model="TEMPmodel.txt" ,
                        monitor=parameters  ,
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

# user  system elapsed 
# 1.198   0.631  32.837 

# save.image("REvironmentLojReg4Models.RData")
# adaptSteps = 500; burnInSteps = 500
# nChains = 2; thinSteps = 31
# numSavedSteps = 1000
# load("REvironmentLojReg4Models.RData")

parameterNames = varnames(codaSamples) # get all parameter names
for ( parName in parameterNames[1:10] ) {
  diagMCMC( codaSamples , parName=parName ,
            saveName=fileNameRoot , saveType="eps" )
}

graphics.off()

summaryInfo <- smryMCMC_HD( codaSamples = codaSamples  )
print(summaryInfo)


# Convert coda-object codaSamples to matrix object for easier handling.
mcmcMat = as.matrix( codaSamples , chains=TRUE )
m = mcmcMat[,"m"]
beta0 = mcmcMat[,"beta0"]
beta1 = mcmcMat[,"beta[1]"]
beta2 = mcmcMat[,"beta[2]"]
beta3 = mcmcMat[,"beta[3]"]
pred1 = mcmcMat[,"pred[1]"]
pred2 = mcmcMat[,"pred[2]"]
pred3 = mcmcMat[,"pred[3]"]
pred4 = mcmcMat[,"pred[4]"]
pred5 = mcmcMat[,"pred[5]"]

# Compute the proportion of m at each index value:
pM1 = sum( m == 1 ) / length( m )
pM2 = sum( m == 2 ) / length( m )
pM3 = sum( m == 3 ) / length( m )
pM4 = 1 - (pM1 + pM2 + pM3)
cat("P(m = 1|D) = ", pM1, "\nP(m = 2|D) = ", pM2, "\nP(m = 3|D) = ", pM3, "\nP(m = 4|D) = ", pM4, "\n")

priorM1 = 0.25
priorM2 = 0.25
priorM3 = 0.25
priorM4 = 0.25

# BF = (prior odds) / (posterior odds)
BF_1vs2 = (pM1/pM2) / (priorM1/priorM2) # H0: beta = beta_{m1}; H1: beta = beta_{m2}
BF_1vs3 = (pM1/pM3) / (priorM1/priorM3) # H0: beta = beta_{m1}; H1: beta = beta_{m3}
BF_1vs4 = (pM1/pM4) / (priorM1/priorM4) # H0: beta = beta_{m1}; H1: beta = beta_{m4}
BF_2vs3 = (pM2/pM3) / (priorM2/priorM3) # H0: beta = beta_{m2}; H1: beta = beta_{m3}
BF_2vs4 = (pM2/pM4) / (priorM2/priorM4) # H0: beta = beta_{m2}; H1: beta = beta_{m4}
BF_3vs4 = (pM3/pM4) / (priorM3/priorM4) # H0: beta = beta_{m3}; H1: beta = beta_{m4}
cat("Bayes Factor_1vs2 =", BF_1vs2, "\nBayes Factor_1vs3 =", BF_1vs3,"\nBayes Factor_1vs4 =", BF_1vs4,
    "\nBayes Factor_2vs3 =", BF_2vs3,"\nBayes Factor_2vs4 =", BF_2vs4,"\nBayes Factor_3vs4 =", BF_3vs4)

# Extract theta values for each model index:
beta1M1 = beta1[ m == 1 ]
beta2M1 = beta2[ m == 1 ]
beta3M1 = beta3[ m == 1 ]

pred1M1 = pred1[ m == 1 ]
pred2M1 = pred2[ m == 1 ]
pred3M1 = pred3[ m == 1 ]
pred4M1 = pred4[ m == 1 ]
pred5M1 = pred5[ m == 1 ]

beta1M2 = beta1[ m == 2 ]
beta2M2 = beta2[ m == 2 ]
beta3M2 = beta3[ m == 2 ]

pred1M2 = pred1[ m == 2 ]
pred2M2 = pred2[ m == 2 ]
pred3M2 = pred3[ m == 2 ]
pred4M2 = pred4[ m == 2 ]
pred5M2 = pred5[ m == 2 ]

beta1M3 = beta1[ m == 3 ]
beta2M3 = beta2[ m == 3 ]
beta3M3 = beta3[ m == 3 ]

pred1M3 = pred1[ m == 3 ]
pred2M3 = pred2[ m == 3 ]
pred3M3 = pred3[ m == 3 ]
pred4M3 = pred4[ m == 3 ]
pred5M3 = pred5[ m == 3 ]

beta1M4 = beta1[ m == 4 ]
beta2M4 = beta2[ m == 4 ]
beta3M4 = beta3[ m == 4 ]

pred1M4 = pred1[ m == 4 ]
pred2M4 = pred2[ m == 4 ]
pred3M4 = pred3[ m == 4 ]
pred4M4 = pred4[ m == 4 ]
pred5M4 = pred5[ m == 4 ]

openGraph(width=8,height=6)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M1 , 
          main=bquote( beta1*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M1 , 
          main=bquote( beta2*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M1 , 
          main=bquote( beta3*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )

openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4,5,6),nrow=3, ncol = 2, byrow=FALSE)  )
plotPost( pred1M1 ,
          main=bquote( pred1*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) ,
          cex.main=1.75 , xlab="Prediction 1"  )
plotPost( pred2M1 ,
          main=bquote( pred2*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) ,
          cex.main=1.75 , xlab="Prediction 2"  )
plotPost( pred3M1 ,
          main=bquote( pred3*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) ,
          cex.main=1.75 , xlab="Prediction 3"  )
plotPost( pred4M1 ,
          main=bquote( pred3*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) ,
          cex.main=1.75 , xlab="Prediction 4"  )
plotPost( pred5M1 ,
          main=bquote( pred3*" when m=1" * " ; p(m=1|D)" == .(signif(pM1,3)) ) ,
          cex.main=1.75 , xlab="Prediction 5"  )

openGraph(width=7,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M2 , 
          main=bquote( beta1*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M2 , 
          main=bquote( beta2*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M2 , 
          main=bquote( beta3*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )


openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4,5,6),nrow=3, ncol = 2, byrow=FALSE)  )
plotPost( pred1M2 ,
          main=bquote( pred1*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) ,
          cex.main=1.75 , xlab="Prediction 1"  )
plotPost( pred2M2 ,
          main=bquote( pred2*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) ,
          cex.main=1.75 , xlab="Prediction 2"  )
plotPost( pred3M2 ,
          main=bquote( pred3*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) ,
          cex.main=1.75 , xlab="Prediction 3"  )
plotPost( pred4M2 ,
          main=bquote( pred3*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) ,
          cex.main=1.75 , xlab="Prediction 4"  )
plotPost( pred5M2 ,
          main=bquote( pred3*" when m=2" * " ; p(m=2|D)" == .(signif(pM2,3)) ) ,
          cex.main=1.75 , xlab="Prediction 5"  )


openGraph(width=7,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M3 , 
          main=bquote( beta1*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M3 , 
          main=bquote( beta2*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M3 , 
          main=bquote( beta3*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )


openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4,5,6),nrow=3, ncol = 2, byrow=FALSE)  )
plotPost( pred1M3 ,
          main=bquote( pred1*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) ,
          cex.main=1.75 , xlab="Prediction 1"  )
plotPost( pred2M3 ,
          main=bquote( pred2*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) ,
          cex.main=1.75 , xlab="Prediction 2"  )
plotPost( pred3M3 ,
          main=bquote( pred3*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) ,
          cex.main=1.75 , xlab="Prediction 3"  )
plotPost( pred4M3 ,
          main=bquote( pred3*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) ,
          cex.main=1.75 , xlab="Prediction 4"  )
plotPost( pred5M3 ,
          main=bquote( pred3*" when m=3" * " ; p(m=3|D)" == .(signif(pM3,3)) ) ,
          cex.main=1.75 , xlab="Prediction 5"  )

openGraph(width=7,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=FALSE) , widths=c(1,2) )
plotPost( m , breaks=seq(0.9,43.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )
plotPost( beta1M4 , 
          main=bquote( beta1*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[1])  )
plotPost( beta2M4 , 
          main=bquote( beta2*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[2])  )
plotPost( beta3M4 , 
          main=bquote( beta3*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) , 
          cex.main=1.75 , xlab=bquote(beta[3])  )

openGraph(width=9,height=5)
par( mar=0.5+c(3,1,2,1) , mgp=c(2.0,0.7,0) )
layout( matrix(c(1,2,3,4,5,6),nrow=3, ncol = 2, byrow=FALSE)  )
plotPost( pred1M4 ,
          main=bquote( pred1*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) ,
          cex.main=1.75 , xlab="Prediction 1"  )
plotPost( pred2M4 ,
          main=bquote( pred2*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) ,
          cex.main=1.75 , xlab="Prediction 2"  )
plotPost( pred3M4 ,
          main=bquote( pred3*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) ,
          cex.main=1.75 , xlab="Prediction 3"  )
plotPost( pred4M4 ,
          main=bquote( pred3*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) ,
          cex.main=1.75 , xlab="Prediction 4"  )
plotPost( pred5M4 ,
          main=bquote( pred3*" when m=4" * " ; p(m=4|D)" == .(signif(pM4,3)) ) ,
          cex.main=1.75 , xlab="Prediction 5"  )

graphics.off()
# ============ Predictive check ============

thresholds <- seq(0.1, 0.95, 0.05)
cf <- array(NA,dim =c(length(thresholds),2))
for (i in 1:(length(thresholds)-1)){
  predProbs <- summaryInfo[7:167,3]
  activityStatus <- array(1, 161) #array(1, 39)
  preds <- data.frame(probSurv = predProbs, activityStatusPred = activityStatus, activityStatusActual = dataTest[,5])
  preds$activityStatusPred[which(preds$probSurv < thresholds[i])] <- 0 # Apply each threshold to estimate those not survived
  cf[i,2] <- confusionMatrix(resp = preds$activityStatusActual, pred = preds$activityStatusPred)$accuracy
  cf[i,1] <- thresholds[i]
}
colnames(cf) <- c("Threshold", "Accuracy")
cf

threshold <- 0.6
preds[which(preds[,1]<threshold),2] <- 0
preds[which(preds[,1]>threshold),2] <- 1

table(preds[,2:3])

confusionMatrix <- function(resp, pred){
  classRes <- data.frame(response = resp , predicted = pred)
  conf = xtabs(~ predicted + response, data = classRes)
  
  accuracy = sum(diag(conf))/sum(conf)
  accuracy
  precision = conf[1,1]/(conf[1,1]+conf[1,2])
  precision
  recall = conf[1,1]/(conf[1,1]+conf[2,1])
  recall
  Fscore = 2*((precision*recall)/(precision+recall))
  Fscore
  return(list(accuracy = accuracy, precision = precision, recall = recall, Fscore = Fscore, conf = conf))
}
 
confusionMatrix(resp = preds$activityStatusActual, pred = preds$activityStatusPred)


# Test accuracy from deep learning: 0.8333333



#------------------LOGISTIC REGRESSION 4 MODELS -------------------------

