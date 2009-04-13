\name{plot.EMM}
\alias{plot.EMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot EMM objects }
\description{
Plot EMM objects as graphs.}
\usage{
plot.EMM(emm,  method=c("graph", "MDS"), data = NULL, 
    parameter=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{method}{ plot as regular graph or position vertices according
  to the dissimilarities between the state centers using
  multi dimensional scaling (\code{"MDS"}).}
  \item{data}{ Project the state centers onto this data. }
  \item{parameter}{ a list of parameters for plotting. }
  \item{\dots}{ further arguments passed on to \code{plot.default}. }
}
%\details{
%}
%\value{
%}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("Traffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, Traffic)

op <- par(mfrow = c(2, 2), pty = "s")
plot(emm, main="Graph (plain)", state_counts=FALSE, transition_prob=FALSE)
plot(emm, main="Graph")
plot(emm, method="MDS", main="MDS projection", 
    xlim=c(-0.35,0.35), ylim= c(-0.35,0.35))
plot(emm, "MDS", Traffic, main = "Projection of centers on data")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
