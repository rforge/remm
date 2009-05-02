
## constructor
EMM <- function(measure="euclidean", threshold=0.2, lambda=0, 
    centroids=identical(tolower(measure), "euclidean")) {
    mm <- new("graphNEL", edgemode="directed")
    structure(list(mm=mm, 
            measure = measure,          ## used dissimilarity measure 
            centroids = centroids,      ## use centroids?
            threshold = threshold,      ## set threshold
            lambda = lambda,            ## learning rate
            lambda_factor = 2^(-lambda),
            current = NA,               ## NA means we are in the reset state
            
            ## these things could go to nodeData but it is more
            ## convenient to have a matrix for centers
            centers = matrix(), ## could be centroids or medoids
            ## we could now sum_x and sum_x2 instead of
            ## centers (like BIRCH) if we only use Euclidean space!
            #sum_x = matrix(),
            #sum_x2 = matrix(),
            counts = numeric(),          ## state counts
            initial_counts = numeric(),  ## for initial trans prob
            var_thresholds = numeric()   ## one threshold per cluster? 
            ## fixme: keep approximate disimilarity matrix (between centers)
        
        ), class ="EMM")
}

size <- function(emm) length(emm$counts)
current_state <- function(emm) emm$current
states <- function(emm) names(emm$counts)
state_counts <- function(emm) emm$counts
state_centers <- function(emm) emm$centers
#state_centers <- function(emm) emm$sum_x/emm$counts

transitions <- function(emm) {
current_state <- function(emm) emm$current
    ed <- edges(emm$mm)
    edges <- NULL
    for(i in 1:length(ed)) {
        to <- as.integer(ed[[i]])
        from <- rep(as.integer(names(ed)[i]), length(to))
        edges <- rbind(edges, cbind(as.character(from), as.character(to)))
    }
    colnames(edges) <- c("from", "to")
    edges
}
