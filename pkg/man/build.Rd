\name{build}
\alias{build}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Build an EMM using New Data}
\description{
Add new data to an EMM.
}
\usage{
build(emm, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{emm} object. }
  \item{newdata}{ a vector (one observation) or a matrix 
    (each row is an observation) }
}
\details{
    The dissimilarity between the new object and the centers of the EMM states
    is calculated. The new observation is assigned to the closest state
    if the dissimilarity value is smaller than the threshold (for the state).
    If no such state exists, a new state is created for the observation.

    The count of the current state is incremented and
    an edge with weight 1 from the current state to the new state is added.
    If this edge already existed, the weight is incremented by one.

    The new state becomes the current state.

    NAs are handled in the data by using only the other dimensions if the data
    for dissimilarity computation.
}
\value{
A new EMM with the data added.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

## load EMMTraffic data
data("EMMTraffic")
EMMTraffic

## create EMM
emm <- EMM(measure="eJaccard", threshold=0.2)

## build model using EMMTraffic data
emm <- build(emm, EMMTraffic)

size(emm)
plot(emm) ## plot needs rgraphviz installed
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
