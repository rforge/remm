
transition <- function(emm, from, to=NULL, 
    type=c("probability", "counts", "log_odds") ){
    type <- match.arg(type)
    
    if(is.null(to) && ncol(from)==2) {
        to <- from[,2]
        from <- from[,1]
    }

    from <- as.character(from)
    to <- as.character(to)

    ## handle NAs as nodes
    nna <- !is.na(from)

    ew <- as.list(rep(NA, length(from)))
    ew[nna] <- lapply(from[nna], FUN = function(x) edgeWeights(emm$mm, x)[[1]])
    
    n <- sapply(1:length(from), FUN = function(i) ew[[i]][to[i]])
    n[is.na(n)] <- 0
    d <- sapply(ew, sum)
    
    if(type=="counts") return(n)
    
    prob <- as.numeric(n/d)
    ## no transition means probability zero
    prob[is.na(prob)] <- 0

    ## states might have no outgoing edges (are leaves)!
    ## these are absorbing states
    l <- leaves(emm$mm, "out")
    prob[from %in% l & to %in% l] <- 1

    if(type=="probability") return(prob)

    ## log odds 
    ## possible outgoing edges n = number of states (includes edge to itself)
    ## for the null-model we suppose they have all the same probability 1/n
    ## log odds = ln(P/(1/n))
    return(log(prob*size(emm)))
}

transition_matrix <- function(emm,
    type=c("probability", "counts", "log_odds")){
    type <- match.arg(type)
    
    #tm <- outer(states(emm), states(emm),
    #FUN = function(x, y) transition(emm, x, y, type=type))
    #dimnames(tm) <- list(states(emm), states(emm))
    #tm

    ## doing it sparse is much more efficient
    m <- matrix(0, ncol=size(emm), nrow=size(emm), 
        dimnames=list(states(emm), states(emm)))
    ew <- edgeWeights(emm$mm, states(emm))
    for(i in 1:length(ew)) m[i, names(ew[[i]])] <- ew[[i]]
    if(type=="counts") return(m)

    rs <- rowSums(m)
    prob <- m/rs
    
    ## we have to handle absorbing states here (row sum is 0)
    absorbing <- which(rs==0)
    prob[absorbing,] <- 0
    for(i in absorbing) prob[i,i] <- 1

    switch(type,
        probability = prob,
        log_odds = log(prob*size(emm))
    )
}


initial_transition <- function(emm, 
    type=c("probability", "counts", "log_odds")){
    type <- match.arg(type)

    switch(type,
        probability = emm$initial_counts / sum(emm$initial_counts),
        counts = emm$initial_counts,
        log_odds = log(emm$initial_counts / sum(emm$initial_counts)* size(emm))
        )
}
