## build for EMM

### alias update
#setMethod("update", signature(object = "EMM"),
#	function(object, newdata, verbose = FALSE) build(object, 
#		newdata, verbose)
#)

## make  newdata a matrix (with a single row)
setMethod("build", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, verbose = FALSE) build(x, 
		as.matrix(rbind(newdata), verbose))
	)

setMethod("build", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, verbose = FALSE) build(x, as.matrix(newdata), 
		verbose)
	)

setMethod("build", signature(x = "EMM", newdata = "matrix"),
	function(x, newdata, verbose = FALSE) {

	    if(verbose) cat("Adding", nrow(newdata) , "observations.","\n")
	    
	    ## cluster all the data (the variable data is in an 
	    ## environment, so there is no need for x <- cluster(x, newdata))
	    cluster(x, newdata, verbose=verbose)

	    ## now update TRACDS (iterate over cluster assignments in last)
	    update(x, x@tnn_d$last, verbose=verbose)
	    
	    invisible(x)
	}
	)
