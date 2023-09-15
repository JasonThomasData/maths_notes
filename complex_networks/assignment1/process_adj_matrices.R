matricesFolder <- "adj_matrices";

if (file.exists(matricesFolder)){
    fileNames <- list.files(matricesFolder);
} else {
    stop("Matrix directory not found");
}

networks <- c();

for (i in 1:length(fileNames)) {
    fileName <- fileNames[i];
    filePath <- sprintf("%s/%s",matricesFolder, fileName);
    
    adjMatrix <- getAdjMatrix(filePath);

    network <- as.network.matrix(adjMatrix, matrix.type="adjacency", directed=FALSE);
    
    set.network.attribute(network,"net id", i);
    
    networks[[i]] <- network;
}

