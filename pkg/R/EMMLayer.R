setMethod("size", signature(x = "EMMLayer"),
	function(x) length(x@initial_counts))

setMethod("states", signature(x = "EMMLayer"),
	function(x) names(x@initial_counts))

setMethod("current_state", signature(x = "EMMLayer"),
	function(x) x@current_state)

setMethod("transitions", signature(x = "EMMLayer"),
	function(x) {
	    m <- smc_countMatrix(x@mm)
	    edges <- apply(which(m>0, arr.ind=T), 
		    MARGIN=2, FUN=function(x) colnames(m)[x])
	    colnames(edges) <- c("from", "to")
	    edges
	}
)


