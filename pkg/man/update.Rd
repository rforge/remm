\name{update}
\alias{update,TRACDS-method}
\alias{update}
\alias{reset}
\alias{reset,TRACDS-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Update a TRACDS temporal structure with new state assignements}
\description{
Add a sequence of new state assignments to a TRACDS object.
}
\usage{
\S4method{update}{TRACDS}(object, newdata, verbose=FALSE, ...)
reset(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,object}{ a \code{TRACDS} object. }
  \item{newdata}{ a vector with a state assignemnt sequence (typically 
  produced by clustering). }
  \item{verbose}{ logical; verbose output? }
  \item{...}{ further arguments.}
}
\details{

    \code{update()}
    adds a new state assignemnt sequenc to the TRACDS object by increasing the
    transition counts and, if needed, creating new states.

    \code{reset} resets the current state to \code{NA} for reading in a
    new sequence. An \code{NA} in \code{newdata}
    also resets the current state.
}
\value{
A reference to the changed TRACDS object with the data added. 
Note: EMM objects store all variable data in an environment which 
enables us to update partial data without copying the whole object. Assignment
will not create a copy! Use the provided method \code{copy()}. 
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{ 
	Class \code{\linkS4class{TRACDS}}, 
	\code{\link{fade}}.}
\examples{
## create an empty TRACDS object
tracds <- TRACDS()
tracds

## update with an cluster assignment sequence
update(tracds, c(1,2,5,5,2))
tracds

plot(tracds, method="graph")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line