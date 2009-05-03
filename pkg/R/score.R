## does newdata come from the EMM?
score <- function(emm, newdata, method = c("prod", "sum", "log_odds"), 
    initial_state_probability = FALSE, match_state="nn",
    plus_one = TRUE, transition_table = FALSE) {

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
    counts <- transition(emm, from, to, type="counts", plus_one=plus_one)
    prob <- transition(emm, from, to, plus_one=plus_one)
    log_odds <- transition(emm, from, to, type="log_odds", plus_one=plus_one)
    
    if(initial_state_probability) {
        from <- c(NA, from)
        to <- c(ssequence[1], to)
        counts <- c(initial_transition(emm, type="counts", 
                plus_one=plus_one)[ssequence[1]], 
            counts)
        prob <- c(initial_transition(emm, plus_one=plus_one)[ssequence[1]], 
            prob)
        log_odds <- c(initial_transition(emm, type="log_odds", 
                plus_one=plus_one)[ssequence[1]], 
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
