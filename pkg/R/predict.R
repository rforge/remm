#######################################################################
# rEMM - Extensible Markov Model (EMM) for Data Stream Clustering in R
# Copyrigth (C) 2011 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


## predict next n states using P^n
setMethod("predict", signature(object = "TRACDS"),
	function(object, n=1, current_state=NULL, 
		probabilities = FALSE) {

		## probabilistic max with random tie breaking
		.prob_max <- function(x) {
			m <- which(x==max(x))
			if(length(m)>1) m <- sample(m,1)
			m
		}

		
		if(is.null(current_state)) current_state <- current_state(object)
		else current_state <- as.character(current_state)


		P <- transition_matrix(object)
		## calculate P^n
		if(n>1) for(i in 1:(n-1)) P <- P%*%P

		prob <- P[current_state,]
		if(probabilities) return(prob)

		## we need random-tie breaking
		return(states(object)[.prob_max(prob)])
	}
)
