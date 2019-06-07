\name{Targets}
\alias{Targets}

\docType{data}

\title{
Targets information in a list.
}

\description{
The information in this data set, was obtained by using \code{getTarget} function. It includes a variable called \code{Targets} which contains targets information, including the compound name, retention time, retention index, together with mass and intensity of the fragments.
}

\usage{data(Targets)}

\format{
  A list with variable number of observations on the following 7 variables:
  \describe{
    \item{compound}{a character vector containing the names of the targets}
    
    \item{ms}{a list of numeric vectors of fragment mass of the targets}
    
    \item{sp}{a list of numeric vectors of fragment intensities of the targets}
    
    \item{rt}{a numeric vector of retention times of the targets}
    
    \item{ri}{a numeric vector of the retention indexed of the targets}
    
    \item{quantFrag}{a numeric vector showing the index of quantifier fragment in \code{ms} and \code{sp} fields.}
    
    \item{sortedFrag}{a list of numeric vectors showing the order of fragments from the most favorable to the list favorable choice for a quantifier.}
    }
}

\details{
This is the data extracted from a library using a target.table. It was obtained by using \code{getTarget} function. The data set is provided as an example to be used for functions in the package. More examples can be found in the reference.
}

%\source{}

\references{
\url{http://omics.georgetown.edu/SIMAT.html}
}

\value{A list}

\examples{
    data(Targets)
}

\keyword{dataset}