\name{recluster}
\alias{recluster_hclust}
\alias{recluster_kmeans}
\alias{recluster_pam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reclustering EMM states}
\description{
Use various clustering methods to recluster states. This is used
to cluster micro clusters into final clusters.
}
\usage{
recluster_hclust(emm, k=NULL, h=NULL, method="average", prune=NULL)
recluster_kmeans(emm, k, ..., prune=NULL)
recluster_pam(emm, k, ..., prune=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{prune}{ prune states with less than \code{prune} counts before
    reclustering. }
  \item{k}{ number of clusters. }
  \item{h}{ heights where the dendrogram tree should be cut.}
  \item{method}{ clustering method used by \code{hclust}. }
  \item{...}{ additional arguments passed on to the clustering algorithm. }
}
\details{
For \code{kmeans} \code{k} can also be a set of initial cluster centers (see
argument \code{centers} for \code{kmeans}).

For \code{hclust} \code{k} or \code{h} can also be a vector. The result is
then a list with several (nested) EMMs, one for each value.
}
\value{
An object of class \code{emm}. Clustering information is available
as the attribute \code{"cluster_info"}. The information provided depends
in the clustering algorithm (see \code{hclust}, \code{kmeans} and \code{pam}).
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## do clustering and plot dendrogram
emm_hc <- recluster_hclust(emm, k=4)

attr(emm_hc, "cluster_info")

## compare original and clustered EMM
op <- par(mfrow = c(2, 2), pty = "m")   
plot(emm, main ="original EMM", data = EMMTraffic)
plot(emm_hc, main ="clustered EMM", data = EMMTraffic)
plot(attr(emm_hc, "cluster_info")$dendrogram)
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
