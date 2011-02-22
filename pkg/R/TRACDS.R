## creator for TRACDS
TRACDS <- function(lambda=0) {
    new("TRACDS", lambda=lambda)
}

## show
setMethod("show", signature(object = "TRACDS"),
	function(object) {
	    cat("tNN with", nstates(object), "states.\n")
	    invisible(NULL)
	})

setMethod("copy", signature(x = "TRACDS"),
	function(x) {

	    r <- new("TRACDS") 
	    
	    ## copy environment
	    r@tracds_d <- as.environment(as.list(x@tracds_d))
	
	    r
	})


setMethod("nstates", signature(x = "TRACDS"),
	function(x) smc_size(x@tracds_d$mm))

setMethod("states", signature(x = "TRACDS"),
	function(x) smc_states(x@tracds_d$mm))

setMethod("current_state", signature(x = "TRACDS"),
	function(x) x@tracds_d$current_state)

setMethod("transitions", signature(x = "TRACDS"),
	function(x) {
	    m <- smc_countMatrix(x@tracds_d$mm)
	    edges <- apply(which(m>0, arr.ind=T), 
		    MARGIN=2, FUN=function(x) colnames(m)[x])
	    colnames(edges) <- c("from", "to")
	    edges
	}
)

setMethod("rare_transitions", signature(x = "TRACDS"),
	function(x, count_threshold)
	transitions(x)[transition(x, transitions(x),
		type="counts") < count_threshold,]
	)

