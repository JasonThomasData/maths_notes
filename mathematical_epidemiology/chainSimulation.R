library("igraph")
library("MASS")

getNumberOffspring <- function(R0, k) {
  # from docs - https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html
  newOffspring <- rnegbin(n=1, mu=R0, theta=k)
  (newOffspring)
}

addNewOffspring <- function(g, offspring, newOffspring, genIndex) {
  sizeOfGraph <- length(V(g))
  g <- igraph::add_vertices(g, nv=newOffspring)
  for (l in (sizeOfGraph+1):(sizeOfGraph+newOffspring)) {
    #print(c("Add node", l))
    newEdge <- c(offspring, l)
    #print(newEdge)
    g <- igraph::add_edges(g, newEdge)
    g <- set_vertex_attr(g,name="gen",index=c(l),value = genIndex+1)
  }
  (g)
}
# tests
# g <- igraph::graph(n=1,edges=c())
# g <- addNewOffspring(g, offspring = 1, newOffspring = 3, 1) # 2:4 in gen 2
# g <- addNewOffspring(g, offspring = 2, newOffspring = 2, 2) # 5:6 in gen 3
# g <- addNewOffspring(g, offspring = 3, newOffspring = 2, 2) # 7:8 in gen 3
# g <- addNewOffspring(g, offspring = 6, newOffspring = 2, 2) # 9:10 in gen 4
# plot(g) # 1->2,3,4; 2->5,6; 3->7,8, 6-> 9,10

generateChains <- function(R0,
                           k,
                           variant,
                           numberOfChains,
                           numberToVisualise,
                           numberOfGenerations) {
  chainSizes = c()
  extinctChains = 0
  graphsToVisualise = c()
  for (i in 1:numberOfChains) {
    print(c("chain", i))
    g <- igraph::graph(n=1,edges=c())  # let there be a root node
    g <- set_vertex_attr(g,index=c(1),name="gen",value = 1)
    for (j in 1:(numberOfGenerations-1)) {
      print(c("gen", j))
      indecesInGeneration = which(vertex_attr(g, "gen") == j)
      if (length(indecesInGeneration-1) == 0) {
        #print("gen empty", j)
        next
      }
      for (n in 1:length(indecesInGeneration)) {
        #print(c("node", indecesInGeneration[n]))
        offspring = indecesInGeneration[n]
        newOffspring <- getNumberOffspring(R0, k)
        #print(c("new offspring", newOffspring))
        if (newOffspring > 0) {
          g <- addNewOffspring(g, offspring, newOffspring, j)
        }
      }
    }
    sizeOfLastGeneration <- length(which(vertex_attr(g, "gen") == numberOfGenerations))
    #print(c("Chain has final gen size:", sizeOfLastGeneration))
    if (sizeOfLastGeneration == 0) {
      #print(c("extinction of chain", i))
      extinctChains = extinctChains + 1
    } else {
      #print(c("survival of chain", i))
    }
    chainSizes = c(chainSizes, length(V(g)))
    
    # This was difficult to put the outputs elsewhere, so it stays
    if (numberToVisualise > 0 && length(V(g)) > 1) {
      numberToVisualise = numberToVisualise - 1
      plot(g,
           layout = layout.reingold.tilford(g, root=1),
           edge.width = 1,
           edge.arrow.width = 0.3,
           vertex.size = 3,
           edge.arrow.size = 0.5,
           vertex.size2 = 1,
           vertex.label = NA,
           asp = 0.5,
           margin = 0)
      title(main = sprintf("%s variant, chain #%d",variant, i))
    }
    
  }
  hist(chainSizes,
       ylim=c(0, numberOfChains),
       main= sprintf("%s variant distribution of chain sizes",
                     variant))
  print(sprintf("The %s variant had an extinction rate of %f",
                variant,
                extinctChains/numberOfChains))
  
}

numberOfChains = 50
numberToVisualise = 6
numberOfGenerations = 6

generateChains(R0=2.5,
               k=0.17,
               variant="Original",
               numberOfChains,
               numberToVisualise,
               numberOfGenerations)

generateChains(R0=5,
               k=0.2,
               variant="Delta",
               numberOfChains,
               numberToVisualise,
               numberOfGenerations)
