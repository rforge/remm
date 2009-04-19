\name{find_states}
\alias{find_states}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find the EMM State for an Observation }
\description{
Finds the EMM states for observations.
}
\usage{
find_states(emm, newdata, match_state=c("exact", "nn"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object.}
  \item{newdata}{ a matrix with observations.}
  \item{match_state}{ find exact or nearest neighbor state.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the name of the matching states.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

find_states(emm, EMMTraffic)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
