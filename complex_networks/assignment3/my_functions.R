getEdgeWeightTotals = function(network, names) {
    edgeWeightTotals = c();
    for (name in names) {
        # Being an undirected graph, we could take .from or .to
        edgesToThisNode = E(network)['.from'(name)]
        combinedWeight = sum(edgesToThisNode$weight)
        edgeWeightTotals = c(edgeWeightTotals, combinedWeight);
    }
    (edgeWeightTotals)
}

getGiantComponent = function(network) {
    components = igraph::clusters(network, mode="weak");
    componentId = which.max(components$csize);
    vertexIds = V(namedNet)[components$membership == componentId];
    giantComponent = igraph::induced_subgraph(network, vertexIds);
    (giantComponent)
}

constructMatrixUsingDegree = function(degreeOfNodes, namedNet, namesInNet, kMax, edgeCount) {
    e_ij = matrix(0, nrow=kMax, ncol=kMax);
    for (edgeIndex in 1:edgeCount) {
        edge = E(namedNet)[edgeIndex];
        nodes = (ends(namedNet, edge[1]));

        name_i = nodes[[1]];
        nodeIndex_i = match(name_i, namesInNet);
        i = degreeOfNodes[nodeIndex_i];

        name_j = nodes[[2]];
        nodeIndex_j = match(name_j, namesInNet);
        j = degreeOfNodes[nodeIndex_j];

        # An undirected graph should have from,to interchangeable
        e_ij[i,j] = e_ij[i,j] + 1;
        e_ij[j,i] = e_ij[j,i] + 1;
    }
    (e_ij)
}

constructMatrixUsingEdgeWeights = function(edgeWeightsOfNodes, namedNet, namesInNet, kMax, edgeCount) {
    e_ij = matrix(0, nrow=kMax, ncol=kMax);
    for (edgeIndex in 1:edgeCount) {
        edge = E(namedNet)[edgeIndex];
        nodes = (ends(namedNet, edge[1]));

        name_i = nodes[[1]];
        nodeIndex_i = match(name_i, namesInNet);
        i = edgeWeightsOfNodes[nodeIndex_i];

        name_j = nodes[[2]];
        nodeIndex_j = match(name_j, namesInNet);
        j = edgeWeightsOfNodes[nodeIndex_j];

        # An undirected graph should have from,to interchangeable
        e_ij[i,j] = e_ij[i,j] + edge$weight;
        e_ij[j,i] = e_ij[j,i] + edge$weight;
    }
    (e_ij)
}


doSumOverKPrime = function(e_k_kPrime, k, kMax) {
    total = 0;
    for (kPrime in 1:kMax) {
        total = total + e_k_kPrime[k,kPrime];
    }
    (total)
}

probabilityKPrimeGivenK = function(e_k_kPrime, kPrime, k, sumOverKPrime) {
    (e_k_kPrime[k,kPrime]/sumOverKPrime)
}

k_nn = function(e_k_kPrime, k, kMax) {
    sumOverKPrime = doSumOverKPrime(e_k_kPrime, k, kMax);
    total = 0;
    for (kPrime in 1:kMax) {
        total = total + kPrime * probabilityKPrimeGivenK(e_k_kPrime, kPrime, k, sumOverKPrime)
    }
    (total)
}

