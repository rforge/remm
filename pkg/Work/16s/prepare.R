library("seqinr")
#library("rEMM")
source("counter.R")


create_stream <- function(file, window=100, overlap=0, 
    word=3, last_window=FALSE, max=0) {
    ## read file
    sequences <- read.fasta(file) 
    
    if(max!=0 && length(sequences)>max) sequences <- sequences[1:max]

    ## count
    cnt <- count_sequences(sequences, 
        window=100, overlap=0, word=word, last_window=FALSE)

    ## report number of windows
    cat(file, "\n")
    cat("window length distr.", "\n")
    print(summary(sapply(cnt, length)))

    ## put together and add start states
    stream <- make_stream(cnt, use_ss = TRUE, ss_val=NA)

    stream
}

Alphaproteobacteria16S <- create_stream("Alphaproteobacteria16s.wri", max=30)
Mollicutes16S <- create_stream("Mollicutes16s.wri", max=30)

save(Mollicutes16S, Alphaproteobacteria16S, file="16S.rda")


