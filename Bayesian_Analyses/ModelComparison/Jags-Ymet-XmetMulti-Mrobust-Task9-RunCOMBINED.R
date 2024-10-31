# Example for Jags-Ymet-XmetMulti-Mrobust.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

#---------------------WITHOUT MODEL UNCERTAINTY------------------------------------- 
#load('~/Documents/MATH2269_Bayesian/tasks/Module6/Task6Chains.RData')
#source("~/Documents/MATH2269_Bayesian/tasks/Module6/Jags-Ymet-XmetMulti-Mrobust-Task6.R")

#plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          #pairsPlot=FALSE , showCurve=FALSE )
#graphics.off()
#---------------------WITHOUT MODEL UNCERTAINTY------------------------------------- 

myData = read.csv("concrete.csv")
yName = "CCS" ; xName = c("Cement",	"Blast.Furnace.Slag",	"Fly.Ash", 	"Water",  	"Superplasticizer", 	"Coarse.Aggregate", 	"Fine.Aggregate" )

fileNameRoot = "Task9"
graphFileType = "eps"
#------------------------------------------------------------------------------- 
genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL  ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault , xPred = xPred) { 
  require(runjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = as.matrix(data[,xName],ncol=length(xName))
  # Do some checking that data make sense:
  if ( any( !is.finite(y) ) ) { stop("All y values must be finite.") }
  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
  cat("\nCORRELATION MATRIX OF PREDICTORS:\n ")
  show( round(cor(x),3) )
  cat("\n")
  flush.console()
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y ,
    Nx = dim(x)[2] ,
    Ntotal = dim(x)[1] ,
    xPred = xPred # Data for predictions
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  # Standardize the data:
  data {
    ym <- mean(y)
    ysd <- sd(y)
    for ( i in 1:Ntotal ) {
      zy[i] <- ( y[i] - ym ) / ysd
    }
    for ( j in 1:Nx ) {
      xm[j]  <- mean(x[,j])
      xsd[j] <-   sd(x[,j])
      for ( i in 1:Ntotal ) {
        zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
      }
    }
    # # Specify the values of indepdenent variable for prediction
    # xPred[1] <- 15
    # xPred[2] <- 75

    # Specify the priors for original beta parameters
    # Prior locations to reflect the expert information
    mu0 <- ym # Set to overall mean a priori based on the interpretation of constant term in regression
    mu[1] <- 0.1 # Cement
    mu[2] <- 0.15 # Blast Furnace Slag
    mu[3] <- 0.1 # FLy Ash
    mu[4] <- -0.5 # Water here this -0.5 is arbitrarirly chosen but the negative sign is due to
                  # the expert thinks that a unit increase in water decreases the CSS.
    mu[5] <- 0.1 # Superplasticizer
    mu[6] <- 0 # Coarse Aggregate
    mu[7] <- 0 # Fine Aggregate

    # Prior variances to reflect the expert information    
    Var0 <- 1 # Set simply to 1
    Var[1] <- 0.01 # Cement
    Var[2] <- 0.1 # Blast Furnace Slag
    Var[3] <- 0.5 # FLy Ash
    Var[4] <- 50 # Water
    Var[5] <- 0.1 # Superplasticizer
    Var[6] <- 0.5 # Coarse Aggregate
    Var[7] <- 0.5 # Fine Aggregate

    # Compute corresponding prior means and variances for the standardised parameters
    muZ[1:Nx] <-  mu[1:Nx] * xsd[1:Nx] / ysd 

    muZ0 <- (mu0 + sum( mu[1:Nx] * xm[1:Nx] / xsd[1:Nx] )*ysd - ym) / ysd 

    # Compute corresponding prior variances and variances for the standardised parameters
    VarZ[1:Nx] <- Var[1:Nx] * ( xsd[1:Nx]/ ysd )^2
    VarZ0 <- Var0 / (ysd^2)

  }
  # Specify the model for standardized data:
  model {
    m ~ dcat( mPriorProb[] )
    mPriorProb[1] <- .95
    mPriorProb[2] <- .05
    for ( i in 1:Ntotal ) {
      zy[i] ~ dt( ifelse(m==1, model1[i] , model2[i]) , 1/zsigma^2 , nu )
      model1[i] <- zbeta0 + sum(zbeta[1:Nx] * zx[i,1:Nx])
      model2[i] <- zbeta0 + sum(zbeta[1:3] * zx[i,1:3]) 
    }
    
    # Priors vague on standardized scale:
    zbeta0 ~ dnorm( muZ0 , 1/VarZ0 )  
    for ( j in 1:Nx ) {
       zbeta[j] ~ dnorm( muZ[j] , 1/VarZ[j] )
    }
    
    # zbeta02 ~ dnorm( muZ0 , 1/VarZ0 )  
    # for ( j in 1:3 ) {
    #   zbeta2[j] ~ dnorm( muZ[j] , 1/VarZ[j] )  
    # }
    zsigma ~ dgamma(0.01,0.01)#dunif( 1.0E-5 , 1.0E+1 )
    nu ~ dexp(1/30.0)

    # Transform to original scale:
    beta[1:Nx] <- ( zbeta[1:Nx] / xsd[1:Nx] )*ysd
    beta0 <- zbeta0*ysd  + ym - sum( zbeta[1:Nx] * xm[1:Nx] / xsd[1:Nx] )*ysd
    sigma <- zsigma*ysd
    # 
    # # Compute predictions at every step of the MCMC
    pred1 <- beta0 + beta[1] * xPred[1] + beta[2] * xPred[2] + beta[3] * xPred[3] + beta[4] * xPred[4]
            + beta[5] * xPred[5] + beta[6] * xPred[6] + beta[7] * xPred[7]

    pred2 <- beta0 + beta[1] * xPred[1] + beta[2] * xPred[2] + beta[3] * xPred[3] 

  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  
  # Must standardize data first...
  #   lmInfo = lm( zy ~ zx )
  #   initsList = list(
  #     beta0 = lmInfo$coef[1] ,   
  #     beta = lmInfo$coef[-1] ,        
  #     sigma = sqrt(mean(lmInfo$resid^2)) ,
  #     nu = 5
  #   )
  
  
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta" ,  "sigma" , "m" , 
                  "zbeta0" , "zbeta" , "zsigma", "nu" , "pred1" , "pred2", "m" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          #inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  # Added by Demirhan
  # if ( !any(is.null(xPred)) ) {
  #   for ( i in 1:nChains){
  #     pred = codaSamples[[i]][,"beta0"]
  #     for ( pName in colnames(xPred) ) {
  #       pred = pred + codaSamples[[i]][,pName]*as.numeric(xPred[pName])
  #     }
  #     codaSamples[[i]]  =cbind(codaSamples[[i]] , as.data.frame(as.matrix(pred)) )
  #   }
  #   codaSamples = as.mcmc.list( codaSamples )
  #   
  # }
  
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

#===============================================================================

smryMCMC = function(  codaSamples , 
                      saveName=NULL) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  paramName = colnames(mcmcMat)
  for ( pName in paramName ) {
    summaryInfo = rbind( summaryInfo , summarizePost( mcmcMat[,pName] ) )
  }
  rownames(summaryInfo) = paramName
  summaryInfo = rbind( summaryInfo , 
                       "log10(nu)" = summarizePost( log10(mcmcMat[,"nu"]) ) )
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  
  return( summaryInfo)
}

#===============================================================================

plotMCMC = function( codaSamples , data , xName="x" , yName="y" ,
                     showCurve=FALSE ,  pairsPlot=FALSE ,
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
  zsigma = mcmcMat[,"zsigma"]
  beta0 = mcmcMat[,"beta0"]
  beta  = mcmcMat[,grep("^beta$|^beta\\[",colnames(mcmcMat))]
  if ( ncol(x)==1 ) { beta = matrix( beta , ncol=1 ) }
  sigma = mcmcMat[,"sigma"]
  nu = mcmcMat[,"nu"]
  log10nu = log10(nu)
  pred1 = mcmcMat[,"pred1"] # Added by Demirhan
  pred2 = mcmcMat[,"pred1"] # Added by Demirhan
  
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
    pairs( cbind( beta0 , beta , sigma , log10nu )[plotIdx,] ,
           labels=c( "beta[0]" , 
                     paste0("beta[",1:ncol(beta),"]\n",xName) , 
                     expression(sigma) ,  expression(log10(nu)) ) , 
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
  # Added to show posterior model probabilities
  
  mcmcMat = as.matrix( codaSamples , chains=TRUE )
  m = mcmcMat[,"m"]
  
  # Compute the proportion of m at each index value:
  pM1 = sum( m == 1 ) / length( m )
  pM2 = 1 - pM1
  
  # Extract beta values for each model index:
  betaM1 = beta[ m == 1, ] # Added by Demirhan
  print(betaM1)
  betaM2 = beta[ m == 2, 1:3]# Added by Demirhan
  beta0M1 = beta0[ m == 1 ]# Added by Demirhan
  beta0M2 = beta0[ m == 2 ]# Added by Demirhan
  predM1 = pred1[ m == 1 ]# Added by Demirhan
  predM2 = pred2[ m == 2 ]# Added by Demirhan
  
  # Extract zbeta values for each model index:
  zbetaM1 = zbeta[ m == 1, ]# Added by Demirhan
  zbetaM2 = zbeta[ m == 2, 1:3 ]# Added by Demirhan
  zbeta0M1 = zbeta0[ m == 1 ]# Added by Demirhan
  zbeta0M2 = zbeta0[ m == 2 ]# Added by Demirhan
  
  #-----------------------------------------------------------------------------
  # Compute R^2 for credible parameters:
  YcorX1 = cor( y , x[,1:7] ) # correlation of y with each x predictor
  Rsq1 = zbetaM1 %*% matrix( YcorX1 , ncol=1 )
  Rsq1 = Rsq1[,1] 
  YcorX2 = cor( y , x[,1:3] ) # correlation of y with each x predictor# Added by Demirhan
  Rsq2 = zbetaM2 %*% matrix( YcorX2 , ncol=1 )# Added by Demirhan
  Rsq2 = Rsq2[,1] 
  #-----------------------------------------------------------------------------
  
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )# Added by Demirhan
  plotPost( m , breaks=seq(0.9,2.1,0.2) , cenTend="mean" , xlab="m" , main="Model Index" )# Added by Demirhan
  
  # Original scale: betaM1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( beta0M1 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(beta[0]) , main="Model 1: Intercept" )
  for ( bIdx in 1:ncol(betaM1) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
    histInfo = plotPost( betaM1[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(betaM1[.(bIdx)]) , main=paste0("Model 1: " , xName[bIdx]) )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( sigma , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(sigma) , main=paste("Scale") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( log10nu , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(log10(nu)) , main=paste("Normality") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( predM1 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(pred) , main="Model 1: Prediction" ) # Added by Demirhan
  
  # panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMarg") )
  # histInfo = plotPost( Rsq1 , cex.lab = 1.75 , showCurve=showCurve ,
  #                      xlab=bquote(R^2) , main=paste("Model 1: Prop Var Accntd") )
  
  # Original scale: betaM2
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( beta0M2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(beta[0]) , main="Model 2: Intercept" )
  for ( bIdx in 1:ncol(betaM2) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
    histInfo = plotPost( beta[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(betaM2[.(bIdx)]) , main=paste0("Model 2: " , xName[bIdx]) )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( sigma , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(sigma) , main=paste("Scale") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( log10nu , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(log10(nu)) , main=paste("Normality") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( Rsq2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(R^2) , main=paste("Model 2: Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMarg") )
  histInfo = plotPost( predM2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(pred) , main="Model 2: Prediction" ) # Added by Demirhan
  
  # Standardized scale: Model 1
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zbeta0M1 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*beta[0]) , main="Model 1: Intercept" )
  for ( bIdx in 1:ncol(betaM1) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
    histInfo = plotPost( zbetaM1[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(z*beta[.(bIdx)]) , main=paste0("Model 1: " , xName[bIdx]) )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zsigma , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*sigma) , main=paste("Scale") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( log10nu , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(log10(nu)) , main=paste("Normality") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( Rsq1 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(R^2) , main=paste("Model 1: Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMargZ") )
  
  # Standardized scale: Model 2
  panelCount = 1
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zbeta0M2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*beta[0]) , main="Model 2: Intercept" )
  for ( bIdx in 1:ncol(betaM2) ) {
    panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
    histInfo = plotPost( zbetaM2[,bIdx] , cex.lab = 1.75 , showCurve=showCurve ,
                         xlab=bquote(z*beta[.(bIdx)]) , main=paste0("Model 2: " , xName[bIdx]) )
  }
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( zsigma , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(z*sigma) , main=paste("Scale") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( log10nu , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(log10(nu)) , main=paste("Normality") )
  panelCount = decideOpenGraph( panelCount , saveName=paste0(saveName,"PostMargZ") )
  histInfo = plotPost( Rsq2 , cex.lab = 1.75 , showCurve=showCurve ,
                       xlab=bquote(R^2) , main=paste("Model 2: Prop Var Accntd") )
  panelCount = decideOpenGraph( panelCount , finished=TRUE , saveName=paste0(saveName,"PostMargZ") )
  
  #-----------------------------------------------------------------------------
}
#------------------------------------------------------------------------------- 

# First run with 0.5 and 0.5 model probabilities.

# Generate the MCMC chain:
numSavedSteps = 15000 
thinSteps = 2
xPred = c(168 ,	42.1 ,	163.8 ,	121.8 ,	5.7 ,	1058.7 ,	780.1  )

startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , xPred = xPred )
stopTime = proc.time()
duration = stopTime - startTime
show(duration)

# save.image("RevironmentConcreteReg2Models.RData")

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames[c(1:10,20:22)] ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
graphics.off()
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:

summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot  )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          pairsPlot=FALSE , showCurve=FALSE  )
#------------------------------------------------------------------------------- 

mcmcMat = as.matrix( mcmcCoda , chains=TRUE )
m = mcmcMat[,"m"]

pM1 = sum( m == 1 ) / length( m )
pM2 = 1 - pM1
cat("P(m = 1|D) = ", pM1, "\nP(m = 2|D) = ", pM2, "\n")


priorM1 = 0.95
priorM2 = 0.05

# BF = (posterior odds) / (prior odds)
BF_1vs2 = (pM1/pM2) / (priorM1/priorM2) # H0: beta = beta_{m1}; H1: beta = beta_{m2}
1/BF_1vs2 
cat("Bayes Factor_2vs1 =", 1/BF_1vs2, "\n")

