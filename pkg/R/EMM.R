
## constructor
EMM <- function(measure="euclidean", threshold=0.2, lambda=0, 
	centroids=identical(tolower(measure), "euclidean")) {

	emm <- new("EMM", measure=measure, threshold=threshold, lambda=lambda,
		centroids=centroids, lambda_factor = 2^(-lambda))	
}
