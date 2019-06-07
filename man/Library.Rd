\name{Library}
\alias{Library}

\docType{data}

\title{
Extracted compound information from an MSL library.
}

\description{
The information in this data set, was obtained by using \code{readMSL} function. It includes a variable called \code{Library} which contains compound names, their retention time and retention index, the mass and the intensity of the fragments in the spectrum of each compound.
}

\usage{data(Library)}

\format{
  A list with the same number of observations on the following 5 variables:
  \describe{
    \item{\code{rt}}{a numeric vector of retention times of the compounds.}
    
    \item{\code{ri}}{a numeric vector of retention indexes of the compounds.}
    
    \item{\code{compound}}{a character vector of name of the compounds.}
        
    \item{\code{ms}}{a list of numeric vectors of mass of fragments for each compound.}

    \item{\code{sp}}{a list of numeric vectors of intensity of fragments for each compound.}
  }
}

\details{
This is the compound information extracted from an \code{MSL} file. It was obtained by using \code{readMSL} function. This library is provided as an example to be used for functions in the package. 
}

%\source{}

\value{A list}

\references{
\url{http://omics.georgetown.edu/SIMAT.html}
}

\examples{
    data(Library)
}

\keyword{Library}