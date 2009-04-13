
build <- function(emm, newdata) {
    
    ## low level graph manipulations w/o copying (work on emm$mm)
    .addEdge <- function(from, to, w=1) {
        emm$mm@edgeL[[from]]$edges <<- c(emm$mm@edgeL[[from]]$edges, 
            which(emm$mm@nodes==to))
        emm$mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<- w
    }

    .incWeight <- function(from, to) {
        emm$mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<-
        emm$mm@edgeData@data[[paste(from,to,sep="|")]]$weight +1
    }

    .addNode <- function(node) {
        emm$mm@nodes <<- c(emm$mm@nodes, node)
        emm$mm@edgeL[[node]]$edges <<- numeric(0)
    }
    
    
    
    ## make sure  newdata is a matrix (maybe a single row)
    if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))

    ## this allows us to add more objects at once
    if(nrow(newdata)>1) {
        for(i in 1:nrow(newdata)) emm <- build(emm, newdata[i,])
        return(emm)
    }

    ## first node?
    if(size(emm)==0) {
        #emm$mm <- addNode("1", emm$mm)
        .addNode("1")
        emm$current <- "1"
        rownames(newdata) <- "1"
        emm$centers <- newdata
        emm$counts["1"] <- 0 ## we only increment for outgoing edges
        
        ## initialize threshold
        emm$var_thresholds["1"] <- emm$threshold

    }else{

        ## we are about to add an outgoing edge
        emm$counts[emm$current] <- emm$counts[emm$current] +1
        
        d <- dist(newdata, emm$centers, method=emm$measure)
        sel <- which.min(d)  

        ## create new node?
        #if(d[sel] > emm$threshold) {
        if(d[sel] > emm$var_thresholds[as.character(sel)]) {
            ## New node
            ## get new node name (highest node number is last entry in count)
            sel <- as.character(as.integer(tail(names(emm$counts),1)) + 1)

            #emm$mm <- addNode(sel, emm$mm)
            .addNode(sel)
            #emm$mm <- addEdge(emm$current, sel, emm$mm, 1)
            .addEdge(emm$current, sel, 1)
            
            rownames(newdata) <- sel
            emm$centers <- rbind(emm$centers, newdata)
            emm$counts[sel] <- 0
            ## initialize threshold
            emm$var_thresholds[sel] <- emm$threshold

            ## update current
            emm$current <- sel

        }else{ 
            ## assign observation to existing node
            sel <- as.character(sel)

            ## add edge or update weight
            if(isAdjacent(emm$mm, emm$current, sel)) {
                
                ## this is slow and complains because the edge already exists
                #suppressWarnings(
                    #        emm$mm <- addEdge(emm$current, sel, emm$mm,
                        #        as.numeric(edgeWeights(emm$mm)[[emm$current]][sel]) +1)
                    #)
                .incWeight(emm$current, sel)

            }else{
                ## new edge
                #emm$mm <- addEdge(emm$current, sel, emm$mm, 1)
                .addEdge(emm$current, sel, 1)
            }


            ## fixme: forget old data
            ## update center (if we use centroids)
            if(emm$centroids) emm$centers[sel,] <- 
            (emm$centers[sel,]*emm$counts[sel] + newdata)/(emm$counts[sel]+1)
            
            ## update counts and current state
            #emm$counts[sel] <- emm$counts[sel] + 1
            emm$current <- sel
        }
    }
    
    emm

}
