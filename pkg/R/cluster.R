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

	    x@tnn_d$last <- character(nrow(newdata))

	    for(i in 1:nrow(newdata)) 
	    {

		nd <- newdata[i,, drop = FALSE]
		if(verbose && i%%50==0) 
		    cat("Added", i, "observations - ",
			nclusters(x), "clusters.\n")

		## reset for rows with all NAs
		if(all(is.na(nd))) {
		    reset(x)
		    x@tnn_d$last[i] <- as.character(NA)
		    next
		}

		## fade cluster structure?
		if(x@lambda>0) 
		    x@tnn_d$counts <- x@tnn_d$counts * x@lambda_factor

		## first cluster
		if(nclusters(x)<1) {
		    sel <- "1"
		    rownames(nd) <- sel
		    x@tnn_d$centers <- nd
		    x@tnn_d$counts[sel] <- 1 
		    ## initialize variable threshold
		    x@tnn_d$var_thresholds[sel] <- x@threshold

		}else{
		    ## find a matching state
		    sel <- find_clusters(x, nd, match_cluster="exact")

		    #inside <- dist(nd, x@tnn_d$centers, 
		    #    method=x@measure) - x@tnn_d$var_thresholds
		    #min <- which.min(inside)
		    #if(inside[min]<=0) sel <- rownames(x@tnn_d$centers)[min]
		    #else sel <- NA

		    ## NA means no match -> create a new node
		    if(is.na(sel)) {
			## New node
			## get new node name (highest node 
			## number is last entry in count)
			sel <- as.character(as.integer(
					tail(names(x@tnn_d$counts),1)) + 1)

			rownames(nd) <- sel
			x@tnn_d$centers <- rbind(x@tnn_d$centers, nd)
			x@tnn_d$counts[sel] <- 1
			## initialize threshold
			x@tnn_d$var_thresholds[sel] <- x@threshold

		    }else{ 
			## assign observation to existing node

			## update center (if we use centroids)
			if(x@centroids) {


			    nnas <- !is.na(nd)
			    x@tnn_d$centers[sel,nnas] <- 
			    (x@tnn_d$centers[sel,nnas] * 
				    x@tnn_d$counts[sel] 
				    + nd[nnas])/(x@tnn_d$counts[sel]+1)
			    nas <- is.na(x@tnn_d$centers[sel,])
			    x@tnn_d$centers[sel,nas] <- nd[nas]

			}

			## update counts 
			x@tnn_d$counts[sel] <- x@tnn_d$counts[sel] + 1
		    }
		}

		x@tnn_d$last[i] <- sel

	    }


	    if(verbose) cat ("Done -", nclusters(x), "clusters.\n")

	    invisible(x)

	}
	)


