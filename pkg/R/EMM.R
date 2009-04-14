
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

