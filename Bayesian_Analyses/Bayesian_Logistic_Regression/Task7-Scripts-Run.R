#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# #.............................................................................
# # Multiple predictors:
set.seed(12332)
myDataF = read.csv( file="EEG_Eye_State_2.csv" ) # full dataset
summary(myDataF)
# Start with 150 observations
myData = myDataF[sample(1:nrow(myDataF), 150, replace = FALSE), ]
# Randomly, divide the dataset into training and test sets.
summary(myData)

Ntrain = floor(0.7*nrow(myData))
trainIdx <- sample(1:nrow(myData),Ntrain,replace = FALSE) # Find the indexes of observations going into the training set
testIdx <- setdiff(1:nrow(myData),trainIdx)               # Find the indexes of observations going into the test set
trainData <- myData[trainIdx,]
testData <- myData[testIdx,]

table(trainData[,15])
summary(trainData)


yName = "eyeDetection" ; xName = c("AF3","F7","F3","FC5","T7","P7","O1","O2","P8","T8","FC6","F4","F8","AF4")
fileNameRoot = "HEEG_Eye_State_no_outliers-"
numSavedSteps=1500 ; thinSteps=11; nChains = 2
# #.............................................................................
graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Task7_Scripts.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
startTime = proc.time()
mcmcCoda = genMCMC( data=trainData , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps, 
                    nChains = nChains )
stopTime = proc.time()
duration = stopTime - startTime
show(duration)
# save.image(file="rEnvironment_11Thin_150obs.RData")
# load(file="rEnvironment_11Thin_150obs.RData")

# save.image(file="rEnvironment_11Thin_Fullobs.RData")
# load(file="rEnvironment_11Thin_Fullobs.RData")
# save.image(file="rEnvironment_11Thin_FullobsRun2.RData")
# load(file="rEnvironment_11Thin_FullobsRun2.RData")
# elapsed: 5807.949

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = c("beta0"  ,  "beta[1]",  "beta[2]" ,  "beta[3]" ,  "beta[4]" ,  "beta[5]"  , "beta[6]"  ,
                   "beta[7]"  , "beta[8]"  , "beta[9]" ,  "beta[10]" , "beta[11]" , "beta[12]"  ,"beta[13]" ,
                   "beta[14]", "guess") #varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
graphics.off()
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

coeffs <- as.vector(summaryInfo[1:15,1])
X <- as.matrix(cbind(rep(1,nrow(testData)),testData[,1:14]))
predProbs <- 1/(1+exp(-X%*%coeffs))

threshold <- 0.5
predClass <- as.numeric(predProbs >= threshold)
table(predClass)
trueClass <- testData[,15]

# ============ Predictive check ============

confusionMatrix <- function(resp, pred){
  classRes <- data.frame(response = resp , predicted = pred)
  confO = xtabs(~ predicted + response, data = classRes)
  conf <- matrix(0,2,2)
  for (i in 1:length(rownames(confO))){
    for(j in 1:length(colnames(confO))){
      conf[as.integer(rownames(confO)[i])+1,(as.integer(colnames(confO)[j]))+1] <- confO[i,j]
    }
  }
  
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

confusionMatrix(resp = trueClass, pred = predClass)
