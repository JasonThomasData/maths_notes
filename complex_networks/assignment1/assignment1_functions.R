
#Function levels
#Given a root node, calculates path length from the root node to every other node in the graph.
#Result will contain negative values for graphs that are not connected.

levels <- function(g, rootnode){

n <- network.size(g)
levels <- rep(-1, times=n)
levels[rootnode]<-0
k <- 0

neighbours <- get.neighborhood(g, rootnode, type="combined")

repeat {stack <- numeric()
        stacksize <- length(neighbours)
        if (length(neighbours)==0) break
        for (j in 1:stacksize){
          i <- neighbours[j]
          #if node i is not yet assigned a level, assign current level and "push" its neighbours onto the stack
          if (levels[i]<0) {
            levels[i]<-k+1
            stack <- c(stack, get.neighborhood(g, i, type="combined"))
            }
          }
        neighbours <- stack
        k <- k+1
        if (length(neighbours)==0) break
       }
levels
}



#Function pathmatrix
#Uses levels to construct the path matrix for a (strongly) connected graph

pathmatrix <- function(g){

n <- network.size(g)
P <- levels(g,1)
for (i in 2:n) {P <- cbind(P, levels(g,i))}
P
}


