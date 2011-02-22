setMethod("remove_clusters", signature(x = "EMM", to_remove = "character"),
	function(x, to_remove) {

		if(length(to_remove)==0) return(x)
		
		to_remove_pos <- states(x) %in% to_remove

		## TRACDS 
		x@tracds_d$mm <- smc_removeState(x@tracds_d$mm, to_remove)
		if(is.element(x@tracds_d$current_state, to_remove)) 
		    x@tracds_d$current_state <- as.character(NA)

		## tNN
		x@tnn_d$centers <- x@tnn_d$centers[!to_remove_pos,]
		x@tnn_d$counts <- x@tnn_d$counts[!to_remove_pos]
		x@tnn_d$var_thresholds <- x@tnn_d$var_thresholds[!to_remove_pos]

		x
	}
)

setMethod("remove_transitions", signature(x = "EMM", 
		from ="matrix", to="missing"),
	function(x, from, to) remove_transitions(x, from[,1], from[,2])
)

setMethod("remove_transitions", signature(x = "EMM", 
		from ="character", to="character"),
	function(x, from, to) {

		if(length(from) != length(to)) 
		    stop("length of from and to do not match!")
		if(length(from)==0) return(x)

		x@tracds_d$mm <- smc_removeTransition(x@tracds_d$mm,from, to)
		x
	}
)

setMethod("remove_selftransitions", signature(x = "EMM"),
	function(x) {
	   x@tracds_d$mm <- smc_removeSelfTransition(x@tracds_d$mm)	
	   x
	}
)
