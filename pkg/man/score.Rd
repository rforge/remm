\name{score}
\alias{score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Score a New Sequence Given an EMM}
\description{
Calculates a score of how likely it is that a new sequence was generated
by the same process as the sequences used to build the EMM.
}
\usage{
score(emm, newdata, method = c("prod", "sum", "log_odds"), match_state = c("nn", "exact"), transition_table = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{newdata}{ sequence to score }
  \item{method}{ method to calculate the score (see details) }
  \item{match_state}{ do the new observations have to fall within
  the threshold of state (\code{"exact"}) or is the nearest neighbor 
  used (\code{"nn"})?}
  \item{transition_table}{ Return the transition table for the sequence 
  through the EMM which is used to calculate the score. }
}
\details{
 Method...
}
\value{
    A scalar score value or a transition table.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
## example to come.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
