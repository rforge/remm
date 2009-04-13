
## probabilistic max with random tie breaking
.prob_max <- function(x) {
    m <- which(x==max(x))
    if(length(m)>1) m <- sample(m,1)
    m
}

## predict next n states using P^n
predict_state <- function(emm, n=1, current=NULL, 
    probabilities = FALSE) {
    if(is.null(current)) current <- emm$current
    else current <- as.character(current)

    
    P <- transition_matrix(emm)
    ## calculate P^n
    if(n>1) for(i in 1:(n-1)) P <- P%*%P
    
    prob <- P[current,]
    if(probabilities) return(prob)

    ## we need random-tie breaking
    return(states(emm)[.prob_max(prob)])
}

