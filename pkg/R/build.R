
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
   
    ## aging is also implemented in age.R
    ## fixme: we might want to reduce the cluster variability (sum_x2)
    ## or the cluster threshold also

    .age <- function() {
        emm$counts <<- emm$counts * emm$lambda_factor
        emm$initial_counts <<- emm$initial_counts * emm$lambda_factor
        emm$mm@edgeData@data <<- lapply(emm$mm@edgeData@data, FUN=function(x) {
                x$weight <- x$weight* emm$lambda_factor
                x
            })
    }

    
    ## make sure  newdata is a matrix (maybe a single row)
    if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))

    ## this allows us to add more objects at once
    if(nrow(newdata)>1) {
        for(i in 1:nrow(newdata)) emm <- build(emm, newdata[i,])
        return(emm)
    }

    ## reset on all NAs
    if(all(is.na(newdata))) return(reset(emm))

    ## aging?
    #if(emm$lambda>0) emm <- age(emm)
    if(emm$lambda>0) .age()

    ## first node?
    if(size(emm)==0) {
        #emm$mm <- addNode("1", emm$mm)
        .addNode("1")
        emm$current <- "1"
        rownames(newdata) <- "1"
        emm$centers <- newdata
        #emm$sum_x <- newdata
        #emm$sum_x2 <- newdata^2
        emm$counts["1"] <- 1 
        emm$initial_counts["1"] <- 1  
        
        ## initialize threshold
        emm$var_thresholds["1"] <- emm$threshold

    }else{

        ## find a matching state
        sel <- find_states(emm, newdata, match_state="exact")

        ## NA means no match -> create a new node
        if(is.na(sel)) {
            ## New node
            ## get new node name (highest node number is last entry in count)
            sel <- as.character(as.integer(tail(names(emm$counts),1)) + 1)

            #emm$mm <- addNode(sel, emm$mm)
            .addNode(sel)
            
            if(!is.na(emm$current)) {
                #emm$mm <- addEdge(emm$current, sel, emm$mm, 1)
                .addEdge(emm$current, sel, 1)
                emm$initial_counts[sel] <- 0  
            }else{
                emm$initial_counts[sel] <- 1  
            }

            rownames(newdata) <- sel
            emm$centers <- rbind(emm$centers, newdata)
            #emm$sum_x <- rbind(emm$sum_x, newdata)
            #emm$sum_x2 <- rbind(emm$sum_x2, newdata^2)
            emm$counts[sel] <- 1
            ## initialize threshold
            emm$var_thresholds[sel] <- emm$threshold

            ## update current
            emm$current <- sel

        }else{ 
            ## assign observation to existing node

            if(!is.na(emm$current)) {
                ## add edge or update weight
                if(isAdjacent(emm$mm, emm$current, sel)) {

                    ## this is slow and complains because the 
                    ## edge already exists
                    #suppressWarnings(
                        #emm$mm <- addEdge(emm$current, sel, emm$mm,
                            #as.numeric(edgeWeights(emm$mm)[[emm$current]][sel]) +1)
                        #)
                    .incWeight(emm$current, sel)

                }else{
                    ## new edge
                    #emm$mm <- addEdge(emm$current, sel, emm$mm, 1)
                    .addEdge(emm$current, sel, 1)
                }
            }else{
                emm$initial_counts[sel] <- emm$initial_counts[sel]+1  
            }

            ## update center (if we use centroids)
            if(emm$centroids) {
                
                nnas <- !is.na(newdata)
                emm$centers[sel,nnas] <- (emm$centers[sel,nnas] * 
                    emm$counts[sel] + newdata[nnas])/(emm$counts[sel]+1)
                nas <- is.na(emm$centers[sel,])
                emm$centers[sel,nas] <- newdata[nas]
                
                #nnas <- !is.na(newdata)
                ## for sum_x and sum_x2 we have additivity
                #emm$sum_x[sel,nnas] <- emm$sum_x[sel,nnas] + newdata[nnas]
                #emm$sum_x2[sel,nnas] <- emm$sum_x2[sel,nnas] + newdata[nnas]^2
                #nas <- is.na(emm$sum_x[sel,])
                #if(any(nas)) {
                    #    emm$sum_x[sel,nas] <- newdata[nas]
                    #emm$sum_x2[sel,nas] <- newdata[nas]^2
                #}
            }

            ## update counts and current state
            emm$current <- sel
            emm$counts[sel] <- emm$counts[sel] + 1
        }
    }
    
    emm

}


## reset the EMM for new sequence
## reset is also done by an observation of all NAs
reset <- function(emm) { 
    emm$current <- NA
    emm
}
