## does newdata come from the EMM?
score <- function(emm, newdata, method = c("prod", "sum", "log_odds"), 
    initial_state_probability = FALSE, match_state="exact", 
    transition_table = FALSE) {

    ## make sure  newdata is a matrix (maybe a single row)
    if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))
    n <- nrow(newdata)

    ## single state
    if(n==1) 
        if(transition_table) return(data.frame(from=NA, to=NA, prob=NA))
        else return(NA)

    ssequence <- find_states(emm, newdata, match_state=match_state)

    ## get probabilites
    from <- ssequence[1:(n-1)]
    to <- ssequence[2:n]
    counts <- transition(emm, from, to, type="counts")
    prob <- transition(emm, from, to)
    log_odds <- transition(emm, from, to, type="log_odds")
    
    if(initial_state_probability) {
        from <- c(NA, from)
        to <- c(ssequence[1], to)
        counts <- c(initial_transition(emm, type="counts")[ssequence[1]], 
            counts)
        prob <- c(initial_transition(emm)[ssequence[1]], 
            prob)
        log_odds <- c(initial_transition(emm, type="log_odds")[ssequence[1]], 
            log_odds)
        ## we have now one transition more
        n <- n+1
    }


    ## only transition table?
    if(transition_table) return(data.frame(from=from, to=to, prob=prob, 
            counts=counts))

    method <- match.arg(method)
    switch(method,
        prod = prod(prob)^(1/n),    
        sum = sum(prob)/n,
        log_odds = sum(log_odds)
    )
}
