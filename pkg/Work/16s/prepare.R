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


    ## put together and add start states
    stream <- make_stream(cnt)

    stream
}

Mollicutes16s <- create_stream("Mollicutes16s-.wri", max=30)
Alphaproteobacteria16s <- create_stream("Alphaproteobacteria16s-.wri", max=30)

save(Mollicutes16s, Alphaproteobacteria16s, "16s.rda")


