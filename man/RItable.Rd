\name{RItable}
\alias{RItable}

\docType{data}

\title{
Extracted RI standard information from a \code{csv} file.
}

\description{
The information in this data set, was obtained by using \code{getRIStandards} function. It includes a variable called \code{RItable} which contains the retention index and measured retention times of the RI standards.
}

\usage{data(RItable)}

\format{
  A data frame including retention times and retention idexes of the RI standards
  \describe{
    \item{\code{rt}}{a numeric vector of retention times of the RI standards.}
    
    \item{\code{ri}}{a numeric vector of retention indexes of the RI standards.}
  }
}

\details{
This is the data extracted from a RI standard information table. It was obtained by using \code{getRIStandards} function. The data set is provided as an example to be used for functions in the package. More examples can be found in the reference.
}

%\source{}

\references{
\url{http://omics.georgetown.edu/SIMAT.html}
}

\value{A data frame}

\examples{
    data(RItable)
}

\keyword{RI table}