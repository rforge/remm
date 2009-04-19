\name{Derwent}
\alias{Derwent}
\docType{data}
\title{Derwent Catchment Data}
\description{
Data sets with river level readings from six catchments of
in the vicinity of the Derwent river in the eastern UK. 
The data was collected daily from 08/25/1986 to 02/23/1995.
The catchments are Long Bridge, Matlock Bath, Chat Sworth,
What Stand Well, Ashford (Wye) and Wind Field Park (Amber).

The owner of the data is the Ridings Area Office of the Environment 
Agency North-East, UK.
}
\usage{data(Derwent)}
\format{
  A matrix of size 1918 days times 6 catchments.
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
UK National River Flow Archive (NRFA),
\url{http://www.ceh.ac.uk/data/nrfa/}

The owner of the data is the Ridings Area Office of the Environment
Agency North-East, UK.
}
%\references{
%}
\examples{
data(Derwent)

i <- 1
plot(Derwent[,i], type="l", main=colnames(Derwent[i]), ylab="Gauged Flows")
}
\keyword{datasets}
