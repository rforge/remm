\name{plot.EMM}
\alias{plot}
\alias{plot.EMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot EMM objects }
\description{
Plot EMM objects as graphs.}
\usage{
\method{plot}{EMM}(x, method=c("MDS", "graph", "state_counts", 
    "transition_counts"), data = NULL, 
    parameter=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{method}{ plot as regular graph or position vertices according
  to the dissimilarities between the state centers using
  multi dimensional scaling (\code{"MDS"}).}
  \item{data}{ Project the state centers onto this data. }
  \item{parameter}{ a list of parameters for plotting. }
  \item{\dots}{ further arguments passed on to \code{plot.default}. }
}
\details{
\code{"MDS"} projects the state centers into 2-dimentional space.
\code{"graph"} produces a graph representation of the EMM.
\code{"state_counts"} produces a barplot for state counts.
\code{"transition_counts"} produces a barplot for transition counts.

The following plotting parameters are currently supported:
\describe{
\item{state_counts}{represent state counts by vertex size?}
\item{arrow_width}{represent transition counts/probabilities by arrow width?}
\item{arrows}{use \code{"counts"} or \code{"probabilities"} for 
    arrow width.}
\item{arrow_width_multiplier, state_size_multiplier}{ Controls the variation
    of vertex sizes and edge widths (default: 1).}
\item{add_labels}{add labels for centers (n/a for \code{type="graph"}).}
\item{mark_clusters}{Use different markers for points depending on the
    state they belong to (only available when a\code{data} is specified).}
\item{mark_states}{a vector of state names to be marked.}
}
}
%\value{
%}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

op <- par(mfrow = c(2, 2), pty = "s")
plot(emm, method="graph", main="Graph (plain)", 
    state_counts=FALSE, transition_prob=FALSE)
plot(emm, method="graph", main="Graph")
plot(emm, method="MDS", main="MDS projection", 
    xlim=c(-0.35,0.35), ylim= c(-0.35,0.35))
plot(emm, method="MDS", EMMTraffic, main = "Projection of centers on data")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
