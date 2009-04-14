
remove_states <- function(emm, to_remove) {
    to_remove <- as.character(to_remove)

    emm$mm <- removeNode(to_remove, emm$mm)
    to_remove <- states(emm) %in% to_remove
    emm$centers <- emm$centers[!to_remove,]
    emm$counts <- emm$counts[!to_remove]
    emm$var_thresholds <- emm$var_thresholds[!to_remove]

    emm
}

