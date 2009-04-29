
counter <- function(x, window=50, overlap=0, word=3, last_window=FALSE) {
    x <- getSequence(x)
    if(last_window) {
        start <- seq(1,length(x), by=window-overlap)
        end <- c(seq(window, length(x), by=window-overlap), length(x))
    }else{
        l <- as.integer(length(x)/(window-overlap)) -1L
        start <- (window-overlap)*(0:l)+1
        end <- start+window-1
    }

    t(sapply(1:length(start), FUN=function(i) 
            count(x[start[i]:end[i]], word=word)))
}

count_sequences <- function(x, window=50, overlap=0, word=2, last_window=FALSE) 
lapply(x, counter, window=window, overlap=overlap, word=word, 
    last_window=last_window)


make_stream <- function(cnt, use_ss=TRUE, ss_val = NA) {
    ## start state
    ss <- rep(ss_val, ncol(cnt[[1]]))

    stream <- matrix(NA, ncol= ncol(cnt[[1]]), nrow=0)
    colnames(stream) <- colnames(cnt[[1]])

    if(use_ss) {
        for(i in 1:length(cnt)) stream <- rbind(stream, ss, cnt[[i]])
    }else{
        for(i in 1:length(cnt)) stream <- rbind(stream, cnt[[i]])
    }
    
    stream
}

