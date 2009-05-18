
merge_states <- function(emm, to_merge = NULL, 
    clustering = FALSE, new_center = NULL) {

    ## handle a clustering
    if(clustering) {
        k <- max(to_merge)
        
        if(!is.null(new_center) && nrow(new_center) != k) 
        stop("new_center has not the right number of rows.")
        
        for(i in 1:k) emm <- merge_states(emm, names(to_merge)[to_merge==i],
            clustering = FALSE, new_center[i,])
        
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

    ## save old state centers
    old_centers <- state_centers(emm)[to_merge,]

    ## create new state
    if(is.null(new_center)) {
        if(emm$centroid) {
            emm$centers[new_state,] <- 
                colSums(old_centers*emm$counts[to_merge])/
                sum(emm$counts[to_merge])
            #emm$sum_x[new_state,] <- colSums(emm$sum_x[to_merge,]) 
            #emm$sum_x2[new_state,] <- colSums(emm$sum_x2[to_merge,]) 
        }else {
			## we take the medoid of the larger cluster
			emm$centers[new_state,] <-
				old_centers[which.max(state_counts(emm)[to_merge]),]
		}
    }else{ 
        ## user supplied new center
        if(identical(length(new_center), ncol(emm$centers))) 
        emm$centers[new_state,] <- new_center
        else stop("new_center does not have the correct length/ncol!")
    }
        
    
    emm$counts[new_state] <- sum(emm$counts[to_merge])
    emm$initial_counts[new_state] <- sum(emm$initial_counts[to_merge])
    
    ## remove states
    to_delete <- states(emm) %in% to_merge[-1]
    emm$centers <- emm$centers[!to_delete,]
    #emm$sum_x <- emm$sum_x[!to_delete,]
    #emm$sum_x2 <- emm$sum_x2[!to_delete,]
    emm$counts <- emm$counts[!to_delete]
    emm$initial_counts <- emm$initial_counts[!to_delete]

    
    ## fix current state
    if(emm$current %in% to_delete) emm$current <- new_state
   
    ## fixme: this only works for metric dissimilarities (distances)
    ## new threshold is max. dissimilarity vom new centroid to any old
    ## centroid + its threshold
    
    d <- dist(state_centers(emm)[new_state,,drop=FALSE], old_centers, 
        method=emm$measure)[1,]
    
    new_threshold <- max(d + emm$var_threshold[names(d)])
    names(new_threshold) <- new_state

    emm$var_thresholds[new_state] <- new_threshold
    
    ## remove var. thresholds
    emm$var_thresholds <- emm$var_thresholds[!to_delete]
    
    emm
}


