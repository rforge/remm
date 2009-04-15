\name{prune}
\alias{prune}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pune States and/or Transitions}
\description{
Simplifies an EMM by removing all states and/or transitions 
which have a count smaller than a given threshold.
}
\usage{
prune(emm, count_threshold = 0.1,
    states = TRUE, transitions = TRUE){
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an object of class \code{EMM}. }
  \item{count_threshold}{ all states/edges with a count below the
  threshold are removed from the model.}
  \item{states}{ remove states?}
  \item{transitions}{ remove transitions?}
}
%\details{
%}
\value{
    Returns an object of class \code{EMM}.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")

## For the example we use a very high learning rate
emm_l <- EMM(measure="eJaccard", threshold=0.2, lambda = 1)
emm_l <- build(emm_l, EMMTraffic)

## show state countes and transition counts
state_counts(emm_l)
transition_matrix(emm_l, type="counts")

## remove all states and edges with a threshold of 0.1
emm_lr <- prune(emm_l, count_threshold=0.1)

## compare graphs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm_l, "graph", main = "EMM with high learning rate")
plot(emm_lr, "graph", main = "Simplified EMM")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
