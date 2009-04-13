\name{EMM}
\alias{EMM}
\alias{states}
\alias{size}
\alias{state_counts}
\alias{state_centers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creator and Accessors for Class EMM}
\description{
    Create an empty EMM object.
}
\usage{
EMM(measure = "euclidean", threshold = 0.2, centroids = TRUE)
states(emm)
size(emm)
state_counts(emm)
state_centers(emm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measure}{ the dissimilarity measure used by 
    the EMM (see \code{dist}
    for available measures) }
  \item{threshold}{ dissimilarity threshold used for assigning a new
    observation to existing states. }
  \item{centroids}{ use centroids for states? If \code{FALSE}, the first
    observation is used to represent a state.}
  \item{emm}{an object of class \code{EMM}. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
The creator \code{EMM} returns an \code{EMM} object which is a list with
the following elements:
  \item{mm}{ a graph representing the Markov Model.}
  \item{centroids}{ a logical indicating if centroids are used. }
  \item{threshold}{dissimilarity threshold used for assignment.}
  \item{current}{name of current state in the EMM.}
  \item{centers}{a matrix containing the centers for the states.}
  \item{counts}{counts for states}
  \item{var_thresholds}{dissimilarity thresholds for individual states (usually
    the same as threshold).}
}
\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In:
{\em ICDM '04: Proceedings of the Fourth IEEE International
Conference on Data Mining,} pp. 371--374.
}
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
emm <- EMM()
emm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
