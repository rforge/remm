\name{predict_state}
\alias{predict_state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Predict a Future State }
\description{
Predict a state/probability distribution over state in n time intervals.
}
\usage{
predict_state(emm, n = 1, current = NULL, probabilities = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{n}{ number of time intervals. }
  \item{current}{ use a specified current state. 
    If \code{NULL}, the EMM's current state is used. }
  \item{probabilities}{ if \code{TRUE},
    instead of the predicted state, the probability distribution is returned.}
}
\details{
Prediction is done using P\^n where P is the transition probability matrix.
Random tie-breaking is used.
}
\value{
    The name of the predicted state or a vector with the probability 
    distribution over all states.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("Traffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, Traffic)

#plot(emm) ## plot graph

## Predict state starting an state 1 after 1, 2 and 100 time intervals
## Note, state 7 is an absorbing state.
predict_state(emm, n=1, current="1")
predict_state(emm, n=2, current="1")
predict_state(emm, n=100, current="1")

## Get probability distribution
predict_state(emm, n=2, current="1", probabilities = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
