prune <- function(emm, count_threshold = 0.1, 
    states = TRUE, transitions = TRUE){

    if(states) emm <- 
    remove_states(emm, names(which(emm$counts < count_threshold)))

    if(transitions) {
        tm <- transition_matrix(emm, type="counts")
        to_remove <- which(tm>0 & tm<count_threshold, arr.ind=TRUE)

        if(nrow(to_remove)>0) {
            from <- states(emm)[to_remove[,1]]
            to <- states(emm)[to_remove[,2]]

            emm <- remove_transition(emm, from, to)
        }

    }

    emm
}

