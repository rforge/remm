## build uses its own implementation of fade! See build.R

fade <- function(emm, t=1, lambda=NULL) {

    if(is.null(lambda)) lambda_factor <- emm$lambda_factor
    else lambda_factor <- 2^(-lambda)

    ## fade counts
    emm$counts <- emm$counts * emm$lambda_factor^t 

    ## fade transition counts
    emm$initial_counts <- emm$initial_counts * emm$lambda_factor^t 
    #edgeWeights(emm$mm) <-  lapply(edgeWeights(emm$mm), "*", fade)
    ## edgeWeights<- not implemented in graph so we have to do it low-level
    emm$mm@edgeData@data <- lapply(emm$mm@edgeData@data, FUN=function(x) {
            x$weight <- x$weight* emm$lambda_factor^t
            x
        })

    emm
}


