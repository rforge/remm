\name{EMM}
\alias{EMM}
\alias{states}
\alias{size}
\alias{state_counts}
\alias{state_centers}
\alias{current_state}
\alias{transitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creator and Accessors for Class EMM}
\description{
    Create an empty EMM object and access its components. Note that
	each cluster in the clustering has always a corresponding state in 
	the EMM. Therefore the terms cluster and state are used 
	interchangeably here.
}
\usage{
EMM(measure = "euclidean", threshold = 0.2, lambda = 0, 
    centroids = identical(tolower(measure), "euclidean"))
size(emm)
states(emm)
state_counts(emm)
state_centers(emm)
transitions(emm)
current_state(emm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{measure}{ the dissimilarity measure used by 
    the EMM (see \code{dist}
    in \pkg{proxy} for available measures) }
  \item{threshold}{ dissimilarity threshold used 
	by the threshold NN clustering algorithm
	for assigning a new
    observation to existing clusters/states. }
  \item{lambda}{ learning rate. if \eqn{lambda>0}, the weight
  of each data point in the stream is decreasing exponentially over time by a
  factor of \eqn{2^{-lambda t}}{2^(-lambda*t)} (see \code{fade()}).}
  \item{centroids}{ use centroids for states? If \code{FALSE}, 
	pseudo medians (first observation of a cluster/state) is used to represent
	a state.  Default is centroids for Euclidean and pseudo medoids otherwise.}
  \item{emm}{an object of class \code{EMM}. }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
The creator \code{EMM} returns an \code{EMM} object which is a list with
the following elements:
  \item{mm}{ a graph representing the first order Markov Model.}
  \item{current}{name of current state in the EMM. \code{NA} means
	no current state.}
  \item{initial_counts}{how often is a state the first state in a sequence
    (used to calculate initial state probabilities for the Markov chain).}
  
  \item{measure}{ used dissimilarity measure. }
  \item{centroids}{ a logical indicating if centroids are used. }
  \item{threshold}{dissimilarity threshold used for assignment.}
  \item{lambda}{learning rate.}
  \item{lambda_factor}{learning rate expressed as a factor. }
  \item{centers}{a matrix containing the cluster/state centers.}
  \item{counts}{number of observations assigned to each cluster/state.}
  \item{var_thresholds}{dissimilarity thresholds for individual states (usually
    the same as threshold).}
}
\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In:
\emph{ICDM '04: Proceedings of the Fourth IEEE International
Conference on Data Mining,} pp. 371--374.
}
%\author{ ~~who you are~~ }
\seealso{ \code{\link{build}} to add observations to the EMM, 
	\code{\link{transition}} to access 
	transition counts/probabilities in the EMM and
	\code{\link[proxy]{dist}} in \pkg{proxy} for available dissimilarity 
	measures.}
\examples{
emm <- EMM()
emm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
