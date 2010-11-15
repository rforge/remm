## add Euclidean Squared Distance

.onLoad <- function(libname, pkgname) {
    pr_DB$set_entry(
	    names = c("Euclidean2"), 
	    FUN = function(x,y) dist(x,y)^2, 
	    PREFUN = NA,
	    POSTFUN = NA,
	    convert = pr_dist2simil,
	    type = "metric",
	    loop = FALSE,
	    C_FUN = FALSE,
	    abcd = FALSE,
	    description="Euclidean Squared Distance."
	    )

    cat("test")
}
