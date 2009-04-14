\name{cluster_states}
\alias{cluster_states}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Use Hierarchical Clustering to Cluster states}
\description{
Applies hierarchical clustering between states.
}
\usage{
cluster_states(emm, method = "average")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{emm}{ an \code{EMM} object. }
  \item{method}{ clustering method (see \code{hclust}). }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
An object of class \code{hclust}.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## do clustering and plot dendrogram
hc <- cluster_states(emm)
plot(hc)

## create a partition with 5 clusters
cl <- cutree(hc, k=5)
cl

## merge states according to the partition
emm_c <- merge_states(emm, cl, clustering = TRUE)

## Note that each state has now an individual threshold (var_thresholds)
emm_c

## compare original and clustered EMM
op <- par(mfrow = c(1, 2), pty = "m")   
plot(emm, main ="original EMM")
plot(emm_c, main ="clustered EMM")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
