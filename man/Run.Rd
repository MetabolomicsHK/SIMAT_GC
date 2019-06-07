\name{Run}
\alias{Run}

\docType{data}

\title{
Extracted peaks from one SIM run.
}

\description{
The information in this data set, was obtained by using \code{readCDF} function. It includes a variable called \code{Run} which contains scans, i.e. mass and intensity pairs, and retention time of the scans for the run.
}

\usage{data(Run)}

\format{
  A list with variable number of observations on the following 4 variables:
  \describe{
    \item{\code{rt}}{a numeric vector of retention times of the scans.}
    \item{\code{sc}}{a numeric vector of scan indexes from 1 to the number of scans.}
    \item{\code{tic}}{a numeric vector of the total ion chromatogram (TIC) of the run.}
    \item{\code{pk}}{a list including same number of items as \code{sc} field where each item of the list includes a matrix with two columns containing the mass and related intensity of each mass}
  }
}

\details{
This is the data extracted from one SIM run. It was obtained by using \code{readCDF} function. The data set is provided as an example to be used for functions in the package. More examples can be found in the reference.
}

%\source{}

\references{
\url{http://omics.georgetown.edu/SIMAT.html}
}

\value{A list}

\examples{
    data(Run)
}

\keyword{dataset}