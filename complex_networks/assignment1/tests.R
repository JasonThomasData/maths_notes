#################################
### Test collectIndices

nodeIndices = 1:7
searchResult = c(-1,2,1,2,-1,-1,1)

result = collectIndices(nodeIndices, searchResult)

assert(as.vector(result$other) == c(1,5,6))
assert(as.vector(result$component) == c(2,3,4,7))

#################################
### Test partitionIndicesByComponent sparse network

adjMatrix = matrix(c(0,1,0,0,0,0,1,
		 1,0,0,0,0,0,0,
		 0,0,0,1,0,0,0,
		 0,0,1,0,1,0,0,
		 0,0,0,1,0,0,0,
		 0,0,0,0,0,0,0,
		 1,0,0,0,0,0,0), 
	       nrow=7,
	       ncol=7)

network <- as.network.matrix(adjMatrix, matrix.type="adjacency", directed=FALSE)

result = partitionIndicesByComponent(network)

assert(result[[1]] == c(1,2,7))
assert(result[[2]] == c(3,4,5))
assert(result[[3]] == c(6))

#################################
### Test getLCPathLengths

components = list()
components[[1]] = c(5,4,3,2,1)
components[[2]] = c(1)
components[[3]] = c(3,2,1)

result = getLCIndices(components)

assert(result == c(5,4,3,2,1))

#################################
### Test chompMinus1

vector = c(-1,-1,1,0,2,-1,1,3)

result = chompMinus1(vector);
assert(result == c(1,0,2,1,3));

#################################
### Test getLCPathMatrix 

adjMatrix = matrix(c(0,1,0,0,0,0,1,
		 1,0,0,0,0,0,0,
		 0,0,0,1,1,0,0,
		 0,0,1,0,1,0,0,
		 0,0,1,1,0,0,0,
		 0,0,0,0,0,0,0,
		 1,0,0,0,0,0,0), 
	       nrow=7,
	       ncol=7)

network <- as.network.matrix(adjMatrix, matrix.type="adjacency", directed=FALSE);

LCIndices = c(3,4,5)

result = getLCPathMatrix(LCIndices, network, 7)

expected = matrix(c(0,1,1,
		1,0,1,
		1,1,0), 
	       nrow=3,
	       ncol=3);

assert(result == expected);

#################################
print("Tests pass")
