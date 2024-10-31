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
# Load the relevant model into R's working memory:
source("Jags-Ymet-XmetMulti-Mrobust-Task9.R")
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

