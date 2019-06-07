\name{target.table}
\alias{target.table}

\docType{data}

\title{
Extracted target information from a \code{csv} file.
}

\description{
The information in this data set, was obtained by using \code{getTargetTable} function. It includes a variable called \code{target.table} which contains compound names, and the  mass of the fragments in the spectrum of each compound selected for SIM analysis.
}

\usage{data(target.table)}

\format{
  A list with the same number of observations on the following 2 variables:
  \describe{
    \item{\code{compound}}{a character vector of name of the compounds.}
        
    \item{\code{ms}}{a list of numeric vectors of mass of fragments for each compound.}
  }
}

\details{
This is the data extracted from a target information table. It was obtained by using \code{getTargetTable} function. The data set is provided as an example to be used for functions in the package. More examples can be found in the reference.
}

%\source{}

\references{
\url{http://omics.georgetown.edu/SIMAT.html}
}

\value{A list}

\examples{
    data(target.table)
}

\keyword{Target Table}