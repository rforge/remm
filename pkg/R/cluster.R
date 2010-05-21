setGeneric("cluster", function(x, newdata,...) standardGeneric("cluster"))


## make  newdata a matrix (with a single row)
setMethod("cluster", signature(x = "tNN", newdata = "numeric"),
	function(x, newdata, verbose = FALSE) cluster(x, 
		as.matrix(rbind(newdata), verbose))
)

setMethod("cluster", signature(x = "tNN", newdata = "data.frame"),
	function(x, newdata, verbose = FALSE) cluster(x, as.matrix(newdata), 
                verbose)
)

setMethod("cluster", signature(x = "tNN", newdata = "matrix"),
	function(x, newdata, verbose = FALSE) {

	    ## this allows us to add more objects at once
	    if(nrow(newdata)>1) {
		last_cluster <- character(nrow(newdata)) 
		
		for(i in 1:nrow(newdata)) 
		{
		    
		    if(verbose && i%%50==0) cat("Added", i, "observations -",size(x), "states.\n")
		    x <- cluster(x, newdata[i,, drop=FALSE])
		    last_cluster[i] <- x@last
		}

		if(verbose) cat ("Done -",size(x), "states.\n")
		x@last <- last_cluster
		
		return(x)
	    }

	    ## reset on all NAs
	    if(all(is.na(newdata))) return()

	    ## fade cluster structure?
	    if(x@lambda>0) x@counts <- x@counts * x@lambda_factor

	    ## first node?
	    if(size(x)==0) {
		sel <- "1"
		rownames(newdata) <- sel
		x@centers <- newdata
		x@counts[sel] <- 1 
		## initialize threshold
		x@var_thresholds[sel] <- x@threshold

	    }else{

		## find a matching state
		sel <- find_states(x, newdata, match_state="exact")

		## NA means no match -> create a new node
		if(is.na(sel)) {
		    ## New node
		    ## get new node name (highest node number is last entry in count)
		    sel <- as.character(as.integer(tail(names(x@counts),1)) + 1)

		    rownames(newdata) <- sel
		    x@centers <- rbind(x@centers, newdata)
		    #x@sum_x <- rbind(x@sum_x, newdata)
		    #x@sum_x2 <- rbind(x@sum_x2, newdata^2)
		    x@counts[sel] <- 1
		    ## initialize threshold
		    x@var_thresholds[sel] <- x@threshold

		}else{ 
		    ## assign observation to existing node

		    ## update center (if we use centroids)
		    if(x@centroids) {

			nnas <- !is.na(newdata)
			x@centers[sel,nnas] <- (x@centers[sel,nnas] * 
				x@counts[sel] + newdata[nnas])/(x@counts[sel]+1)
			nas <- is.na(x@centers[sel,])
			x@centers[sel,nas] <- newdata[nas]

			#nnas <- !is.na(newdata)
			## for sum_x and sum_x2 we have additivity
			#x@sum_x[sel,nnas] <- x@sum_x[sel,nnas] + newdata[nnas]
			#x@sum_x2[sel,nnas] <- x@sum_x2[sel,nnas] + newdata[nnas]^2
			#nas <- is.na(x@sum_x[sel,])
			#if(any(nas)) {
			#    x@sum_x[sel,nas] <- newdata[nas]
			#x@sum_x2[sel,nas] <- newdata[nas]^2
			#}
		    }

		    ## update counts
		    x@counts[sel] <- x@counts[sel] + 1
		}
	    }
	    
	    x@last <- sel
	    x

	}
	)


