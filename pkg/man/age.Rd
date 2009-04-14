\name{age}
\alias{age}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Age Old Data Points}
\description{
Reduces the weight of old observations in the data stream. If an EMM has
a learning rate greater than 0, age is called before each new observation
is added to the EMM. Therefore, calling \code{age} directly is typically not 
be necessary.
}
\usage{
age(emm, t = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an object of class \code{EMM}. }
  \item{t}{ number of time intervals (default=1) }
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

## build a regular EMM for comparison
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## compare the transition matrix
transition_matrix(emm)
transition_matrix(emm_l)

## compare graphs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm, "graph", main = "regular EMM")
plot(emm_l, "graph", main = "EMM with high learning rate")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
