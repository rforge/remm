
transition <- function(emm, from, to, 
    type=c("probability", "counts", "log_odds") ){
    type <- match.arg(type)
    
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

transition_matrix <- function(emm, type="probability") {
    tm <- outer(states(emm), states(emm),
    FUN = function(x, y) transition(emm, x, y, type=type))
    dimnames(tm) <- list(states(emm), states(emm))
    tm
}

