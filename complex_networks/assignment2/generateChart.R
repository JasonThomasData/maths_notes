# run this in Rstudio, it will be interactive

library('network', 'networkD3')

setwd("~/src/maths_notes/complex_networks/assignment2/")

jazz <- as.matrix(read.table("jazz_edgelist.txt"));

src <- jazz[,1] -1;
target <- jazz[,2] -1;
networkData <- data.frame(src, target);

# This generates an interactive plot. Best to run this as a script and then sa
networkD3::simpleNetwork(networkData, opacity = 0.5)
