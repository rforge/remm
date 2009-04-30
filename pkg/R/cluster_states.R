
cluster_states <- function(emm, method="average") {
    d <- dist(state_centers(emm), method = emm$measure)
    hclust(d, method=method)
}


