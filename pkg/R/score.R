## does newdata come from the EMM?

setMethod("score", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition_probability = FALSE, transition_table = FALSE) 
	score(x, as.matrix(rbind(newdata)), method, 
		match_state, plus_one, initial_transition_probability, transition_table)
)

setMethod("score", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition_probability = FALSE, transition_table = FALSE) 
	score(x, as.matrix(newdata), method, 
		match_state, plus_one, initial_transition_probability, transition_table)
)

setMethod("score", signature(x = "EMM", newdata = "matrix"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition_probability = FALSE, transition_table = FALSE) {

		## make sure  newdata is a matrix (maybe a single row)
		if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))
		n <- nrow(newdata)

		## single state
		if(n==1) 
		if(transition_table) return(data.frame(from=NA, to=NA, prob=NA))
		else return(NA)

		ssequence <- find_states(x, newdata, match_state=match_state)

		## get probabilites
		from <- ssequence[1:(n-1)]
		to <- ssequence[2:n]
		counts <- transition(x, from, to, type="counts", plus_one=plus_one)
		prob <- transition(x, from, to, plus_one=plus_one)
		log_odds <- transition(x, from, to, type="log_odds", plus_one=plus_one)
		
		n <- n-1	## we only have n-1 transitions!


		if(initial_transition_probability) {
			from <- c(NA, from)
			to <- c(ssequence[1], to)
			counts <- c(initial_transition(x, type="counts", 
					plus_one=plus_one)[ssequence[1]], 
				counts)
			prob <- c(initial_transition(x, plus_one=plus_one)[ssequence[1]], 
				prob)
			log_odds <- c(initial_transition(x, type="log_odds", 
					plus_one=plus_one)[ssequence[1]], 
				log_odds)
			n <- n+1	## now we have n transitions again
		}


		## only transition table?
		if(transition_table) return(data.frame(from=from, to=to, prob=prob, 
				counts=counts))

		method <- match.arg(method)
		switch(method,
			prod = prod(prob)^(1/n),    
			sum = sum(prob)/n,
			log_odds = sum(log_odds)
		)
	}
)
