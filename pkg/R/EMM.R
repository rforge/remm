
## constructor
EMM <- function(threshold=0.2, measure="euclidean", 
	centroids=identical(tolower(measure), "euclidean"), lambda=0) {

	new("EMM", measure=measure, threshold=threshold, 
		centroids=centroids, lambda=lambda)
}

### deep copy
setMethod("copy", signature(x = "EMM"),
	function(x) {

	    r <- new("EMM",
		    threshold = x@threshold,
		    measure = x@measure,
		    centroids = x@centroids,
		    lambda = x@lambda,
		    lambda_factor = x@lambda_factor)

	    ## copy environments
	    r@tnn_d <- as.environment(as.list(x@tnn_d))
	    r@tracds_d <- as.environment(as.list(x@tracds_d))

	    r
	})



## show
setMethod("show", signature(object = "EMM"),
        function(object) {
            cat("EMM with", size(object), "states/clusters.\n", 
                    "Measure:", object@measure, "\n",
                    "Threshold:", object@threshold, "\n",
                    "Centroid:", object@centroids, "\n",
                    "Lambda:", object@lambda, "\n"
                    )
            invisible(NULL)
        })

## size delegates to nclusters (in tNN)
setMethod("size", signature(x = "EMM"),
	        function(x) nclusters(x)
		)


