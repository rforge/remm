## does newdata come from the EMM?
score <- function(emm, newdata, method = c("prod", "sum", "log_odds"), 
    match_state=c("nn","exact"), transition_table = FALSE) {

    ## make sure  newdata is a matrix (maybe a single row)
    if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))
    n <- nrow(newdata)

    ## single state
    if(n==1) 
        if(transition_table) return(data.frame(from=NA, to=NA, prob=NA))
        else return(NA)

    ## cross-dissimilarities
    d <- dist(newdata, emm$centers, method=emm$measure)
    
    match_state <- match.arg(match_state)
    if(match_state=="exact"){ 
        ## exact: we reduce the dissimilarities by the thresholds
        ## neg. distance means that we are within the threshold
        d <- sapply(1:ncol(d), FUN = function(i) d[,i]-emm$var_thresholds[i])
    } 

    ## get sequence of states using the nearest cluster
    nn <- apply(d, MARGIN=1, which.min)
    ssequence <- states(emm)[nn]

    ## get probabilites
    from <- ssequence[1:(n-1)]
    to <- ssequence[2:n]
    prob <- transition(emm, from, to)
    log_odds <- transition(emm, from, to, type="log_odds")
    
    ## find points outside the threshold for "exact"
    ## d contains dissimilarities - thresholds!
    if(match_state=="exact") {
        too_far <- sapply(1:length(nn), FUN = function(i) d[i,nn[i]]) > 0 

        ## fix transition_table
        from[too_far[1:(n-1)]] <- NA
        to[too_far[2:n]] <- NA
        prob[is.na(from) | is.na(to)] <- 0
    }

    ## only transition table?
    if(transition_table) return(data.frame(from=from, to=to, prob=prob, 
            log_odds=log_odds))

    method <- match.arg(method)
    switch(method,
        prod = prod(prob)^(1/n),    
        sum = sum(prob)/n,
        log_odds = sum(log_odds)
    )
}
