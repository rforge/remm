
## reset the EMM for new sequence
## reset is also done by an observation of all NAs
setMethod("reset", signature(x = "TRACDS"), function(x) { 
		x@tracds_d$current_state <- as.character(NA)
		x
	}
)
