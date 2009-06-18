
setMethod("transition", signature(x = "EMMLayer", 
		from = "matrix", to = "missing"),
	function(x, from, to, 
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){

		to <- from[,2]
		from <- from[,1]

		transition(x, from, to, type, plus_one)
	}
)
		

setMethod("transition", signature(x = "EMMLayer", 
		from = "character", to = "character"),
	function(x, from, to, 
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){
		type <- match.arg(type)

		## handle NAs as nodes
		nna <- !is.na(from)

		ew <- as.list(rep(NA, length(from)))
		ew[nna] <- lapply(from[nna], FUN = function(z) 
			edgeWeights(x@mm, z)[[1]])

		n <- sapply(1:length(from), FUN = function(i) ew[[i]][to[i]])
		n[is.na(n)] <- 0

		## start with a uniform prior distribution
		if(plus_one) n <- n+1

		if(type=="counts") return(n)

		d <- sapply(ew, sum)
		if(plus_one) d <- d + size(x)
		prob <- as.numeric(n/d)
		## no transition means probability zero
		prob[is.na(prob)] <- 0

		## states might have no outgoing edges (are leaves)!
		## these are absorbing states
		if(!plus_one) {
			l <- leaves(x@mm, "out")
			prob[from %in% l & to %in% l] <- 1
		}

		if(type=="probability") return(prob)

		## log odds 
		## possible outgoing edges n = number of states (includes edge to itself)
		## for the null-model we suppose they have all the same probability 1/n
		## log odds = ln(P/(1/n))
		return(log(prob*size(x)))
	}
)


setMethod("transition_matrix", signature(x = "EMMLayer"),
	function(x,
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){
		type <- match.arg(type)

		#tm <- outer(states(x), states(x),
			#FUN = function(x, y) transition(x, x, y, type=type))
		#dimnames(tm) <- list(states(x), states(x))
		#tm

		## doing it sparse is much more efficient
		m <- matrix(0, ncol=size(x), nrow=size(x), 
			dimnames=list(states(x), states(x)))
		ew <- edgeWeights(x@mm, states(x))
		for(i in 1:length(ew)) m[i, names(ew[[i]])] <- ew[[i]]
		if(plus_one) m <- m+1

		if(type=="counts") return(m)

		rs <- rowSums(m)
		prob <- m/rs

		## we have to handle absorbing states here (row sum is 0)
		absorbing <- which(rs==0)
		prob[absorbing,] <- 0
		for(i in absorbing) prob[i,i] <- 1

		switch(type,
			probability = prob,
			log_odds = log(prob*size(x))
		)
	}
)


setMethod("initial_transition", signature(x = "EMMLayer"),
	function(x, 
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){
		type <- match.arg(type)

		ic <- x@initial_counts
		if(plus_one) ic <- ic+1

		switch(type,
			probability = ic / sum(ic),
			counts = ic,
			log_odds = log(ic / sum(ic)* size(x))
		)
	}
)
