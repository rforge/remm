\name{remove_states}
\alias{remove_states}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Remove States from an EMM }
\description{
Remove states from an EMM.
}
\usage{
remove_states(emm, to_remove)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object.}
  \item{to_remove}{ Names of states to remove.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a new EMM with removed states.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("Traffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, Traffic)

## remove state 3
emm_r3 <- remove_states(emm, "3")

## compare EMMs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm, main = "original EMM")
plot(emm_r3, main = "state 3 removed")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
