\name{remove}
\alias{remove_states}
\alias{remove_transitions}
\alias{remove_selftransitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Remove States/Tranditions from an EMM }
\description{
Remove states/transitions from an EMM.
}
\usage{
remove_states(emm, to_remove)
remove_transitions(emm, from, to=NULL)
remove_selftransitions(emm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object.}
  \item{to_remove}{ Names of states to remove.}
  \item{from, to}{ Names of states for removing transitions. If to is 
    \code{NULL} from has to contain a matrix/data.frame with two columns
    (from and to state names).}
}
\details{
\code{remove_selftransitions} removes all transitions from a state to itself.
}
\value{
Returns a new EMM with removed states/transitions.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## remove state 3
emm_rs3 <- remove_states(emm, "3")

## remove transition 5->2
emm_rt52 <- remove_transitions(emm, "5", "2")

## compare EMMs
op <- par(mfrow = c(2, 2), pty = "m")
plot(emm, "graph", main = "original EMM")
plot(emm_rs3, "graph", main = "state 3 removed")
plot(emm_rt52, "graph", main = "transition 5->2 removed")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
