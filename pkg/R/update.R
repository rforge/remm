## update for TRACDS for cluster assignments

### alias update
setMethod("update", signature(object = "TRACDS"),
	function(object, newdata, ...) {

	    x <- object
	    tracds_d <- x@tracds_d

	    ## inline function to increase performance
	    .addState <- function(name) {
		## expand?
		if(tracds_d$mm@top < 1) {
		    old_size <- nstates(x)

		    new_size <- old_size*2L
		    new_counts <- matrix(0, ncol=new_size, nrow=new_size)
		    new_counts[1:old_size, 1:old_size] <- tracds_d$mm@counts
		    tracds_d$mm@counts <- new_counts

		    new_initial_counts <- numeric(new_size)
		    new_initial_counts[1:old_size] <- tracds_d$mm@initial_counts
		    names(new_initial_counts)[1:old_size] <- names(tracds_d$mm@initial_counts)
		    tracds_d$mm@initial_counts <- new_initial_counts

		    new_unused <- new_size:1
		    new_unused[(old_size+1):length(new_unused)] <- tracds_d$mm@unused
		    tracds_d$mm@unused <- new_unused

		    tracds_d$mm@top <- old_size+tracds_d$mm@top
		}

		## add node
		pos <- tracds_d$mm@unused[tracds_d$mm@top]
		tracds_d$mm@unused[tracds_d$mm@top] <- NA
		tracds_d$mm@top <- tracds_d$mm@top-1L
		names(tracds_d$mm@initial_counts)[pos] <- name

		tracds_d$mm@initial_counts[pos] <- 0 

		pos
	    }


	    ## get position of current date in matrix
	    pos_current <- which(states(x) == current_state(x))

	    ## iterate over cluster assignments in newdata
	    for(sel in newdata) {

		## cluster returns NA if we start a new sequence.
		if(is.na(sel)) {
		    pos_current <- numeric(0)
		    next
		}

		## fade TRACDS structure?
		if(x@lambda>0) tracds_d$mm <- smc_fade(tracds_d$mm, 
			x@lambda_factor) 

		## state exists?
		pos_new <- which(states(x) == sel)

		## no: create state
		if(!length(pos_new)) pos_new <- .addState(sel)

		## add transition
		## no current state?
		if(!length(pos_current)) {
		    tracds_d$mm@initial_counts[pos_new] <- tracds_d$mm@initial_counts[pos_new] + 1 
		}else{
		    tracds_d$mm@counts[pos_current, pos_new] <- tracds_d$mm@counts[pos_current, pos_new] + 1
		}

		## update current_state
		pos_current <- pos_new
	    }

	    ## save the last state as current
	    tracds_d$current_state <- sel

	    invisible(x)
	}
	)
