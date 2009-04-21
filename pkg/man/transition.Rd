\name{transition}
\alias{transition}
\alias{transition_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Access Transition Probabilities/Counts in an EMM }
\description{
Calculates individual transition probabilities/counts 
or a complete transition matrix
in an EMM.
}
\usage{
transition(emm, from, to=NULL, type = c("probability", "counts", "log_odds"))
transition_matrix(emm, type = c("probability", "counts", "log_odds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an object of class \code{EMM}.}
  \item{from, to}{ Names a states. If to is \code{NULL}, from has
  to contain a matrix/data.frame with two columns (a from column and a 
  to column as returned by \code{transitions}).}
  \item{type}{ What should be calculated? }
}
%\details{
%}
\value{
    A scalar (for \code{transition}) or a square matrix 
    (for \code{transition_matrix}).
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## get transition matrix
transition_matrix(emm)
transition_matrix(emm, type="count")

## access individual transition probability (state 1 -> 2)
transition(emm, "1","2")

## get counts for all existing transitions
tr <- transitions(emm)
cbind(tr, counts=transition(emm, tr, type="counts"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
