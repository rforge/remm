## build uses its own implementation of age! See build.R

age <- function(emm, t=1, lambda=NULL) {

    if(is.null(lambda)) lambda_factor <- emm$lambda_factor
    else lambda_factor <- 2^(-lambda)

    ## age counts
    emm$counts <- emm$counts * emm$lambda_factor^t 

    ## age transition counts
    #edgeWeights(emm$mm) <-  lapply(edgeWeights(emm$mm), "*", age)
    ## edgeWeights<- not implemented in graph so we have to do it low-level
    emm$mm@edgeData@data <- lapply(emm$mm@edgeData@data, FUN=function(x) {
            x$weight <- x$weight* emm$lambda_factor^t
            x
        })

    emm
}


