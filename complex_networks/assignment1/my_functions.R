getAdjMatrix = function(filePath) {
    adjTable = read.table(file = filePath, header = FALSE);
    adjMatrix = as.matrix(adjTable);
    rownames(adjMatrix) <- colnames(adjMatrix); # so that we can check symmetry
    (adjMatrix);
}

# I tried for a while to display network graphs on a grid. It was easier to save 
# them to files and then combine them using a bash script.
# See the bash scipt makeGrid.sh
plotAllNetworks = function(networks) {
    networkVizFolder = "networkViz";
    for (i in 1:length(networks)) {
        fileName = sprintf("chart%s.png", i);
        filePath = sprintf("%s/%s",networkVizFolder, fileName);
        title = sprintf("gvole net %s", i);
        png(filePath);
        plot(networks[[i]], main=title);
        dev.off();
    }
}

collectIndices = function(uncollectedIndices, searchResult) {
    componentIndices = c();
    otherIndices = c();
    for (i in uncollectedIndices) {
        if (searchResult[[i]] == -1) {
            otherIndices = c(otherIndices, i);
        } else {
            componentIndices = c(componentIndices, i);
        }
    }
    (list(component=componentIndices, other=otherIndices));
}

# This seemed simpler, in the end, compared to using igraph. Various reasons
partitionIndicesByComponent = function(network) {
    components = list();
    comp_i = 1;
    
    i = 1;
    searchResult = levels(network, i);
    nodeIndices = 1:length(searchResult); # These are assigned once and not again
    
    indices = collectIndices(nodeIndices, searchResult);
    componentIndices = indices$component;
    components[[comp_i]] = componentIndices;
    uncollectedIndices = indices$other;
    
    while (length(uncollectedIndices) > 0) {
        i = uncollectedIndices[1];
        searchResult = levels(network, i);
        
        indices = collectIndices(uncollectedIndices, searchResult);
        componentIndices = indices$component;
        
        comp_i = comp_i + 1;
        components[[comp_i]] = componentIndices;
        uncollectedIndices = indices$other;
    }
    (components)
}

getLCIndices = function(components) {
    largest_i = 1;
    largestNumberOfPaths = 0;
    for (i in 1:length(components)) {
        component = components[[i]];
        if (length(component) > largestNumberOfPaths) {
            largestNumberOfPaths = length(component);
            largest_i = i;
        }
    }
    (components[[largest_i]])
}

chompMinus1 = function(vector) {
    chompedVector = c();
    for (i in 1:length(vector)) {
        if (vector[[i]] != -1) {
            chompedVector = c(chompedVector, vector[[i]]);
        }
    }
    (chompedVector)
}

getLCPathMatrix = function(LCIndices, network, n) {
    numberOfIndices = length(LCIndices);
    pathMatrix = matrix(, nrow=numberOfIndices, ncol=0);
    for (i in LCIndices) {
        pathLengths = levels(network, i);
        pathLengths = chompMinus1(pathLengths);
        pathMatrix = cbind(pathMatrix, pathLengths);
    }
    (pathMatrix)
}

getDataForNetwork = function(network, dataForAllNetworks) {
    # Let LC be largest component
    n = network.size(network);
    e = network.edgecount(network);
    indicesByComponent = partitionIndicesByComponent(network);
    numberOfComponents = length(indicesByComponent);
    LCIndices = getLCIndices(indicesByComponent);
    LCPathMatrix = getLCPathMatrix(LCIndices, network, n);
    numberOfNodesInLC = length(LCIndices);
    fractionOfNodesInLC = numberOfNodesInLC/n;
    averageDegree = mean(degree(network, gmode="graph"));
    averagePathLengthForLC = mean(LCPathMatrix);
    
    dataForNetwork = list(net_id=get.network.attribute(network, "net id"),
                          nodes=n,
                          edges=e,
                          components=numberOfComponents,
                          lc_node_share=fractionOfNodesInLC,
                          avg_degree=averageDegree, 
                          avg_path_length_lc=averagePathLengthForLC );
    
    (dataForNetwork)
}

getDataForAllNetworks = function(networks) {
    dataForAllNetworks = data.frame(net_id = integer(),
                                    nodes = integer(),
                                    edges = integer(),
                                    comonents = integer(),
                                    lc_node_share = integer(),
                                    avg_degree = numeric(),
                                    avg_path_length_lc = numeric()
                                   );

    for (i in 1:length(networks)) {
        dataForOneNetwork = getDataForNetwork(networks[[i]]);
        dataForAllNetworks = rbind(dataForOneNetwork,dataForAllNetworks);
    }
    (dataForAllNetworks[order(dataForAllNetworks$net_id),])
}

