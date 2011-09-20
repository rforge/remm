\name{combine}
\alias{c}
\alias{c,EMM-method}
\title{Combining EMM Objects}
\description{
    Combines two or more EMMs into a single object.
}
\usage{
%c(x, \ldots, recursive = FALSE)
\S4method{c}{EMM}(x, ..., recursive = FALSE)
}
\arguments{
  \item{x}{ first \code{EMM} object. Note that this object will be changed 
  by the function! }
  \item{\dots}{ further objects of the same class as \code{x} to be combined.}
  \item{recursive}{ a logical. If \code{recursive=TRUE}, 
    the function recursively
    descends through lists combining all their elements into a
    vector.}
}
\value{
  Returns invisibly an object of the same class as \code{EMM}.
}
\seealso{
  \code{\link{EMM-class}},
}
\examples{
data("16S")

emm1 <- EMM("Kullback", threshold=0.1)
build(emm1, Mollicutes16S+1)

emm2 <- EMM("Kullback", threshold=0.1)
build(emm2, Alphaproteobacteria16S+1)

emm12 <- copy(emm1)
c(emm12, emm2)


emm12r <- copy(emm12)
recluster_tNN(emm12r)

op <- par(mfrow = c(1, 2), pty = "s")
plot(emm12, method="graph", main="Two EMMs")
plot(emm12r, method="graph", main="Two EMMs (reclustered)")
par(op)



}
\keyword{manip}
