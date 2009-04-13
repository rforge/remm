
cluster_states <- function(emm, method="average") {
    d <- dist(emm$centers, method = emm$measure)
    hclust(d, method=method)
}


