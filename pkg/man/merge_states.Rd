\name{merge_states}
\alias{merge_states}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge States of an EMM }
\description{
Merge several states of an EMM into a single state preserving 
counts/probabilities in the EMM.}
\usage{
merge_states(emm, to_merge, clustering = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{to_merge}{ vector of names of the states to merge. The
      name of the first state in \code{to_merge}
      is used as the name for the new state representing the merged states.}
  \item{clustering}{ is \code{to_merge} a clustering? }
}
%\details{
%}
\value{
A new EMM with the states merged.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("Traffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, Traffic)
states(emm)

## merge states 1-3
emm_m123 <- merge_states(emm, c("1", "2", "3"))
states(emm_m123)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
