setMethod("state_counts", signature(x = "tNN"),
	function(x) x@counts)

setMethod("state_centers", signature(x = "tNN"),
	    function(x) x@centers)

setMethod("size", signature(x = "tNN"),
	    function(x) nrow(x@centers))

setMethod("states", signature(x = "tNN"),
	    function(x) rownames(x@centers))

