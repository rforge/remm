
setMethod("find_states", signature(x = "tNN", newdata = "numeric"),
	function(x, newdata, match_state=c("exact", "nn"))
	find_states(x, as.matrix(rbind(newdata), match_state))
)	

setMethod("find_states", signature(x = "tNN", newdata = "data.frame"),
	function(x, newdata, match_state=c("exact", "nn")) 
	find_states(x, as.matrix(newdata), match_state))

setMethod("find_states", signature(x = "tNN", newdata = "matrix"),
	function(x, newdata, match_state=c("exact", "nn")) {

		match_state <- match.arg(match_state)

		## cross-dissimilarities
		d <- dist(newdata, state_centers(x), method=x@measure)

		.which.min_NA <- function(x) {
			m <- which.min(x)
			if(length(m)==0) m <- NA
			m
		}

		if(match_state=="nn") return (states(x)[apply(d, MARGIN=1, 
					.which.min_NA)])

		## exact matching using thresholds (using the largest margin)
		## NA ... no match

		## subtract threshold and take the smallest value if <=0
		d <- d - matrix(x@var_thresholds,
			ncol=length(x@var_thresholds), nrow=nrow(d), byrow=TRUE)

		closest <- states(x)[apply(d, MARGIN=1, .which.min_NA)]
		closest_val <- apply(d, MARGIN=1, min)
		closest[closest_val>0] <- NA
		closest
	}
)
