EMM_from_clustering <- function(clustering, centers, 
    measure="euclidean", threshold=0.1, centroids = TRUE) {
    
    emm <- EMM(measure=measure, threshold=threshold, centroids = centroids)

    k <- max(clustering)

    ## nodes
    emm$mm <- addNode(as.character(1:k), emm$mm)
    
    ## edges
    m <- matrix(0, ncol=k, nrow=k)
    for(i in (1:length(clustering)-1)) m[clustering[i], clustering[i+1]] <- 
        m[clustering[i], clustering[i+1]] +1

    edges <- data.frame(
        from = rep(1:k,k),
        to = as.vector(sapply(1:k, rep, k)),
        weight = as.vector(m)
    )
    edges <- edges[edges$weight>0,]

    emm$mm <- addEdge(as.character(edges$from), as.character(edges$to), 
        emm$mm, edges$weight)

    ## counts
    emm$counts <- table(clustering)

    ## replace counts
    emm$centers <- centers
    
    ## fixme: get var thresholds
    emm$var_thresholds <- rep(threshold, k)

    ## initial count
    emm$initial_counts <- rep(0, k)
    emm$initial_counts[clustering[1]] <- 1

    ## current
    emm$current <- as.character(tail(clustering, 1))

    emm
}

