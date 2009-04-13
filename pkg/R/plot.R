
plot.EMM <- function(emm, method=c("graph", "MDS"), data = NULL, 
    parameter=NULL, ...) {
    method <- match.arg(method)
    
    p <- .get_parameters(list(
            state_counts=TRUE,
            transition_probability=TRUE,
            lwd_multiplier=1,
            statesize_multiplier=1,
            add_labels = TRUE,
            mark_clusters = TRUE
        ), parameter)



    if(method=="graph") {
        if(!require("Rgraphviz")) stop ("Package Rpgraphviz needed!")

        if(p$state_counts){
            nAttrs <- list(width=.5 +
                emm$counts/max(emm$counts)*p$statesize_multiplier)
            ## setting line width for arrows does not seem to be implemented
            pl <- plot(emm$mm, recipEdges="distinct", 
                nodeAttrs = nAttrs, ...)

        }else plot(emm$mm, recipEdges="distinct", ...)
        
        if(p$transition_probability)
        ## redraw arrows with different width
        tmp <- sapply(AgEdge(pl), FUN = function(x) {
                lwd <- 1+transition(emm, tail(x), head(x))*
                    p$lwd_multiplier*4
                lines(x, lwd=lwd, len=.1)
            }
        )
    }

    else if(is.null(data)){
        d <- dist(emm$centers, method=emm$measure)
        mds <- cmdscale(d, eig=TRUE, add=TRUE)

        ## add arrows
        ed <- edges(emm$mm)
        edges <- NULL
        for(i in 1:length(ed)) {
            to <- as.integer(ed[[i]])
            from <- rep(as.integer(names(ed)[i]), length(to))
            edges <- rbind(edges, cbind(as.character(from), as.character(to)))         
        }
        
        x <- mds$points
        dimnames(x) <- list(states(emm), NULL)

        lwd <- 1
        cex <- 2
        
        ## use cex for point size
        if(p$state_counts) cex <- 2+emm$counts/max(emm$counts)*
            p$statesize_multiplier*5
        
        ## lwd for arrows
        if(p$transition_probability) lwd <- 
            1+transition(emm, edges[,1], edges[,2])*
                p$lwd_multiplier*4

        ## empty plot
        plot(mds$points, xlab="Dimension 1", ylab="Dimension 2", type="n", ...)
        
        ## arrows whines about zero length arrows
        arrows_fromto <- cbind(
            from_x = x[edges[,1],1], from_y= x[edges[,1],2],
            to_x= x[edges[,2],1],to_y=x[edges[,2],2]
        )
     
        ## make arrows shorterby% shorter
        shortenby <- 0.1
        arrows_fromto[,1:2] <- arrows_fromto[,1:2] + 
        (arrows_fromto[,3:4] - arrows_fromto[,1:2]) *(shortenby/2)
        arrows_fromto[,3:4] <- arrows_fromto[,3:4] + 
        (arrows_fromto[,1:2] - arrows_fromto[,3:4]) *(shortenby/2)


        suppressWarnings(
            arrows(arrows_fromto[,1], arrows_fromto[,2], 
                arrows_fromto[,3],arrows_fromto[,4],
                length=0.15, col="grey", angle=20, lwd=lwd)
        )
        
        ## overplot points and text
        points(mds$points, cex=cex)
        labels <- states(emm)
        cex <- cex/2
        ## make sure double digit labels fit
        cex <- cex * (strwidth("8")/strwidth(labels))
        text(mds$points, labels=labels, cex=cex)

    
    } else {
        d <- dist(rbind(emm$centers, data), method=emm$measure)
        mds <- cmdscale(d, eig=TRUE, add=TRUE)
        centers <- mds$points[1:nrow(emm$centers),]
        allpoints <- mds$points[-c(1:nrow(emm$centers)),]
        
        
        ## points
        if(p$mark_clusters){
            d2 <- dist(emm$centers, data, method=emm$measure)
            point_center <- apply(d2, MARGIN=2, which.min)
            ## make sure we stay below 25 for pch
            while(any(point_center>25)) point_center[point_center>25] <- 
                point_center[point_center>25] -25
            plot(allpoints, xlab="Dimension 1", ylab="Dimension 2", 
                col="grey", pch=point_center, ...)
        }else{
            plot(allpoints, xlab="Dimension 1", ylab="Dimension 2", 
                col="grey", ...)
        }
        
        cex <- 1
        
        ## use cex for point size (scale: 1...3)
        if(p$state_counts) cex <- 1+emm$counts/max(emm$counts)*2
        
        ## centers
        points(centers, col="red", pch="+", cex=cex)
        #points(centers, col="red", pch=1:size(emm), cex=cex)
        if(p$add_labels) text(centers, labels=states(emm), pos=3)
    }

}

