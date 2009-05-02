
## medoid is defined as the object of a cluster, whose average 
## dissimilarity to all the objects in the cluster is minimal
## min_{m\inC}(1/n_C sum_{i\inC\m}(d(i,m))
.find_medoids <- function(d, k, cl) {
    dm <- as.matrix(d)
    sapply(1:k, FUN =function(i){
            take <- cl==i    
            names(which.min(colSums(dm[take,take, drop=FALSE])))
        })
}

## hierarchical clustering
recluster_hclust <- function(emm, k=NULL, h=NULL,  method="average", 
    prune=NULL) {

    if(!is.null(prune)) emm <- prune(emm, count_threshold = prune, 
        transitions = FALSE)

    d <- dist(state_centers(emm), method = emm$measure)
    hc <- hclust(d, method=method)
    cl <- cutree(hc , k=k, h=h)
    
    if(is(cl, "matrix")) emm <- lapply(1:ncol(cl), 
        FUN=function(i) 
        {
            if(!emm$centroids) 
            new_center <- state_centers(emm)[.find_medoids(d, k, cl[,i]),]
            ## centroids are handled by merge_states!
            else new_center <- NULL
            merge_states(emm, cl[,i], clustering=TRUE, new_center = new_center)
        })
    else{ 
        if(!emm$centroids) 
        new_center <- state_centers(emm)[.find_medoids(d, k, cl),]
        else new_center <- NULL
        emm <- merge_states(emm, cl, clustering=TRUE,  new_center = new_center)
    }
    
    attr(emm, "cluster_info") <- list(clustering=cl, dendrogram=hc)
    emm
}

## k-means (euclidean)
recluster_kmeans <- function(emm, k, ..., prune=NULL) {
    
    if(!is.null(prune)) emm <- prune(emm, count_threshold = prune, 
        transitions = FALSE)

if(!identical(tolower(emm$measure), "euclidean")) warning(
        paste("Using k-means implies Euclidean distances but the EMM uses:", 
            emm$measure))

    cl <- kmeans(state_centers(emm), centers = k, ...)
   
    emm <- merge_states(emm, cl$cluster, clustering=TRUE, new_center=cl$centers)
    
    attr(emm, "cluster_info") <- cl
    emm
}

## Partitioning around medoids (k-medians)
recluster_pam <- function(emm, k, ..., prune=NULL) {

    if(!is.null(prune)) emm <- prune(emm, count_threshold = prune, 
        transitions = FALSE)

    d <- dist(state_centers(emm), method = emm$measure)
    cl <- pam(d, k=k, ...)

    medoids <- state_centers(emm)[cl$medoids,]
    emm <- merge_states(emm, cl$clustering, clustering=TRUE, new_center=medoids)
    
    attr(emm, "cluster_info") <- cl
    emm
}


