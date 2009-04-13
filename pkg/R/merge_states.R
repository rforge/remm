
merge_states <- function(emm, to_merge = NULL, clustering = FALSE) {
    
    
    ## handle a clustering
    if(clustering) {
        k <- max(to_merge)
        for(i in 1:k) emm <- merge_states(emm, names(to_merge)[to_merge==i])
        return(emm)
    }

    ## nothing to do
    if(length(to_merge) < 2) return(emm)

    new_state <- to_merge[1]

    ## handle edges between states to be merged
    new_w <- sum(sapply(edgeWeights(emm$mm, to_merge), 
            FUN = function(e) sum(e[names(e) %in% to_merge])))

    ## merge states into new_state
    emm$mm <- combineNodes(to_merge, emm$mm, new_state)
    
    ## add edge new_state -> new_state
    if(new_w >0) emm$mm <- addEdge(new_state, new_state, emm$mm, new_w)


    ## combine centroids and counts
    comb_centers <- emm$centers[to_merge,]
    comb_counts <-  emm$counts[to_merge]

    new_count <- sum(comb_counts)
    names(new_count) <- new_state

    if (emm$centroids) new_center <- 
        colSums(comb_centers*comb_counts)/new_count
    else new_center <- emm$centers[new_state]

    ## save new state
    emm$centers[new_state, ] <- new_center
    emm$counts[new_state] <- new_count
    
    ## remove states
    to_delete <- states(emm) %in% to_merge[-1]
    emm$centers <- emm$centers[!to_delete,] 
    emm$counts <- emm$counts[!to_delete]
    

    ## fix current state
    if(emm$current %in% to_delete) emm$current <- new_state
   
    ## fixme: this only works for metric dissimilarities (distances)
    ## new threshold is max. dissimilarity vom new centroid to any old
    ## centroid + its threshold
    
    d <- dist(rbind(new_center), comb_centers, method=emm$measure)[1,]
    
    new_threshold <- max(d + emm$var_threshold[names(d)])
    names(new_threshold) <- new_state

    emm$var_thresholds[new_state] <- new_threshold
    emm$var_thresholds <- emm$var_thresholds[!to_delete]
    
    emm
}


