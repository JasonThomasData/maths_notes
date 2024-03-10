unionIsFullNetwork = function(communities, fullNet) {
    cummulativeUnion = communities[[1]];
    for (i in 2:length(communities)) {
        cummulativeUnion = union(cummulativeUnion, communities[[i]])
    }
    # Union of communities should be same as graph. Same number of nodes confirms this
    (length(cummulativeUnion) == vcount(fullNet))
}

intersectionIsEmpty = function(communities) {
    cummulativeIntersection = communities[[1]];
    for (i in 2:length(communities)) {
        cummulativeIntersection = intersect(cummulativeIntersection, communities[[i]])
    }
    # Intersection of communities should be the empty set
    (length(cummulativeIntersection) == 0)
}

sharesKMinus1Nodes = function(communityToMerge, communityToCompare, kMinus1) {
    (length(intersect(communityToMerge, communityToCompare)) >= kMinus1)
}

kCliquePercolation = function(net, k) {
    # If any community shares k-1 nodes with any other, then those communities can be merged
    # To start with, all communities are single k-cliques

    # Since we're using a linked list object, it would be wise to use a while loop and pop
    # elements from the top of the list as needed. If the current clique under-iteration
    # cannot be merged to another then we leave it as-is but increase the cursor + 1.
    # In this case the community under-iteration

    # This process will necessarily drop any node that doesn't belong to a k-clique
    
    # Make each k-clique a community
    
    communities <- as.pairlist(cliques(net, min=k, max=k));

    cursor = 1; # Place in the list
    merged = FALSE
    while (cursor < length(communities)) {
        communityToMerge = communities[[cursor]];
        for (j in (cursor+1):length(communities)) {
            communityToCompare = communities[[j]];
            if (sharesKMinus1Nodes(communityToMerge, communityToCompare, 2)) {
                communities[[j]] = union(communityToMerge, communityToCompare);
                communities <- communities[-cursor];
                merged <- TRUE;
                break;
            }
        }
        if (!merged) {
            cursor <- cursor + 1;
        } else {
            merged <- FALSE;
        }
    }    
    (communities)
}

findOverlaps = function(communities) {
    overlaps = c(); # these elements appear in more than one community
    
    for (i in 1:(length(communities)-1)) {
        for (j in (i+1):length(communities)) {
            overlap = as.vector(intersect(communities[[i]], communities[[j]]));
            overlaps = c(overlaps, overlap);
         }
    }
    (overlaps);
}

