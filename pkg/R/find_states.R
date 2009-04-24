find_states <- function(emm, newdata, match_state=c("exact", "nn")) {

    match_state <- match.arg(match_state)

    ## make sure  newdata is a matrix (maybe a single row)
    if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))

    ## cross-dissimilarities
    d <- dist(newdata, emm$centers, method=emm$measure)

    .which.min_NA <- function(x) {
        m <- which.min(x)
        if(length(m)==0) m <- NA
        m
    }

    if(match_state=="nn") return (states(emm)[apply(d, MARGIN=1, 
                .which.min_NA)])

    ## exact matching using thresholds (using the largest margin)
    ## NA ... no match

    ## subtract threshold and take the smallest value if <=0
    d <- d - matrix(emm$var_thresholds,
        ncol=length(emm$var_thresholds), nrow=nrow(d), byrow=TRUE)

    closest <- states(emm)[apply(d, MARGIN=1, .which.min_NA)]
    closest_val <- apply(d, MARGIN=1, min)
    closest[closest_val>0] <- NA
    closest
}

