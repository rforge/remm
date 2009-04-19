
remove_states <- function(emm, to_remove) {
    to_remove <- as.character(to_remove)

    if(length(to_remove)==0) return(emm)

    emm$mm <- removeNode(to_remove, emm$mm)
    to_remove <- states(emm) %in% to_remove
    emm$centers <- emm$centers[!to_remove,]
    emm$counts <- emm$counts[!to_remove]
    emm$var_thresholds <- emm$var_thresholds[!to_remove]

    emm
}

remove_transitions <- function(emm, from, to) {
    from <- as.character(from)
    to <- as.character(to)
    
    if(length(from) != length(to)) stop("length of from and to do not match!")
    if(length(from)==0) return(emm)

    emm$mm <- removeEdge(from, to, emm$mm)
    emm
}
