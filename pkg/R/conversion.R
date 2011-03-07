## create a tNN clustering from k-means, etc.
## This is TRAC (without DS)

TRAC <- function(x) {
    if(is(x, "kmeans")) {
	counts <- x$size
	k <- length(counts)
	centers <- x$centers
	### FIXME: get radius of clusters from clustering
	thresholds <- rep(1, k)
	order <- x$cluster
    
    ### PAM
    }else if(is(x, "partition")) {
	counts <- x$clusinfo[,"size"]
	k <- length(counts)
	centers <- x$medoids
	thresholds <- x$clusinfo[,"max_diss"]
	order <- x$cluster
    }else stop("Clustering type not supported (only support for kmeans and pam!)")

    emm <- new("EMM")

    ## create tNN
    states <- as.character(1:k)
    names(counts) <- states

    emm@tnn_d$centers <- centers
    emm@tnn_d$counts <- counts
    emm@tnn_d$var_thresholds <- thresholds

    ## update TRACDS
    ## make sure the order of states corresponds to tNN
    emm@tracds_d$mm <- smc_addState(emm@tracds_d$mm, states)
    update(emm, order)

    emm
}

