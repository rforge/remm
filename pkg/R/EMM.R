
## constructor
EMM <- function(measure="euclidean", threshold=0.2, lambda=0, centroids=TRUE) {
    mm <- new("graphNEL", edgemode="directed")
    structure(list(mm=mm, 
            measure = measure, 
            centroids = centroids, 
            threshold = threshold, 
            lambda = lambda,
            lambda_factor = 2^(-lambda),
            current = NULL,
            
            ## these things could go to nodeData but it is more
            ## convenient to have a matrix for centers
            centers = NULL, ## could be centroids or medoids
            counts = vector(),
            var_thresholds = vector()   ## one threshold per cluster? 
            ## fixme: keep approximate disimilarity matrix (between centers)
        
        ), class ="EMM")
}


states <- function(emm) {
    names(emm$counts)
}

size <- function(emm) {
    length(emm$counts)
}

state_counts <- function(emm) {
    emm$counts
}

state_centers <- function(emm) {
    emm$centers
}


transitions <- function(emm) {
    ed <- edges(emm$mm)
    edges <- NULL
    for(i in 1:length(ed)) {
        to <- as.integer(ed[[i]])
        from <- rep(as.integer(names(ed)[i]), length(to))
        edges <- rbind(edges, cbind(as.character(from), as.character(to)))
    }
    colnames(edges) <- c("from", "to")
    as.data.frame(edges)
}
