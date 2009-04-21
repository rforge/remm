prune <- function(emm, count_threshold = 1, 
    states = TRUE, transitions = TRUE){

    if(states) emm <- 
    remove_states(emm, rare_states(emm, count_threshold=count_threshold))

    if(transitions) emm <- remove_transitions(emm, 
        rare_transitions(emm, count_threshold=count_threshold))

    emm
}

rare_states <- function(emm, count_threshold = 1) 
    names(which(emm$counts < count_threshold))

rare_transitions <- function(emm, count_threshold = 1) 
    transitions(emm)[transition(emm, transitions(emm), 
            type="counts") < count_threshold,]
