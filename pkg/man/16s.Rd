\name{16s}
\alias{16s}
\alias{Alphaproteobacteria16s}
\alias{Mollicutes16s}
\docType{data}
\title{Count Data for 16s rRNA Sequences}
\description{
This data set contains count data for 16s rRNA sequences for
Alphaproteobacteria and Mollicutes. The counts for 30 sequences
for each class were obtained by counting the occurrence of triplets
of amino acids in windows of length 100 without any overlap. To separate
sequences a row of dummy count of -10 is used.}
\usage{data("16s")}
\format{
    \code{Alphaproteobacteria16s} and \code{Mollicutes16s} are matrices
    with about 449 rows and 64 (number of possible triplets) columns. 
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The raw sequence information was obtained from the 
National center for biotechnology information (NCBI) website at
http://www.ncbi.nih.gov/
}
%\references{
%}
\examples{
data("16s")

emm <- EMM("Kullback", threshold=0.1)
emm <- build(emm, Mollicutes16s+1)

plot(emm, "graph")

## start state for sequences have an initial state probability >0
it <- initial_transition(emm)
it[it>0]
}
\keyword{datasets}
