
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

	    ## low level graph manipulations w/o copying (work on x@mm)
	    .addEdge <- function(from, to, w=1) {
		x@mm@edgeL[[from]]$edges <<- c(x@mm@edgeL[[from]]$edges, 
			which(x@mm@nodes==to))
		x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<- w
	    }

	    .incWeight <- function(from, to, w=1) {
		x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<-
		x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight + w
	    }

	    .addNode <- function(node) {
		x@mm@nodes <<- c(x@mm@nodes, node)
		x@mm@edgeL[[node]]$edges <<- numeric(0)
	    }

	    ## aging is also implemented in fade.R
	    ## fixme: we might want to reduce the cluster variability (sum_x2)
	    ## or the cluster threshold also

	    .fade <- function() {
		# is done in cluster.R
		#x@counts <<- x@counts * x@lambda_factor
		x@initial_counts <<- x@initial_counts * x@lambda_factor
		x@mm@edgeData@data <<- lapply(x@mm@edgeData@data, 
			FUN=function(z) {
			    z$weight <- z$weight* x@lambda_factor
			    z
			})
	    }


	    ## this allows us to add more objects at once
	    if(nrow(newdata)>1) {
		for(i in 1:nrow(newdata)) 
		{
		    if(verbose && i%%50==0) cat("Added", i, "observations -",
			    size(x), "states.\n")
		    x <- build(x, newdata[i,, drop=FALSE])
		}

		if(verbose) cat ("Done -",size(x), "states.\n")
		return(x)
	    }


	    ## prepare
	    ## reset on all NAs
	    if(all(is.na(newdata))) return(reset(x))

	    ## fade cluster structure?
	    #if(x@lambda>0) x <- fade(x)
	    if(x@lambda>0) .fade()

	    ## cluster
	    x <- cluster(x, newdata)

	    ## get assignment
	    sel <- x@last

	    ## create state?
	    if(is.na(x@initial_counts[sel])) {
		.addNode(sel)
		x@initial_counts[sel] <- 0 
	    }

	    ## add transition
	    ## no current state
	    if(is.na(x@current_state)) {
		x@initial_counts[sel] <- x@initial_counts[sel]+1 
	    }else{
		if(isAdjacent(x@mm, x@current_state, sel)) {
		    .incWeight(x@current_state, sel, 1)
		}else{
		    .addEdge(x@current_state, sel, 1)
		}
	    }

	    ## update current_state
	    x@current_state <- sel

	    x

	}
	)


