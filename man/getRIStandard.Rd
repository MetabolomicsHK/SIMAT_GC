%% getRIStandard function %%
\name{getRIStandard}
\alias{getRIStandard}

\title{Get the RI standard information}

\description{This function generates a table which can be used for RI calibration when calling the \code{getRI} function It retrieves the retention indexes of RI standards and their retention times in a table, e.g. data frame.}

\usage{
getRIStandard(file.name = character(), path = getwd())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{file.name}{a string including the full name of a \code{csv} file including the information of RI standards in two columns, one for retention time and one for retention index.}
    
    \item{path}{a string inlcuding the full path to the RI standard \code{.csv} file lication, optional}
}

\details{The input of this function is a table in \code{.csv} format RItable. The file should include the information of RI standards in two columns, one for retention times and one for retention index of the standards. The information is usually extracted by running a mixture of RI standards for calibration. The output is a table, i.e. a data frame, including the retention time and retention index of the RI standards which can be further used when calling \code{getRI} function.
}

\value{A data frame including retention times and retention idexes of the RI standards} 

\seealso{\code{\link{getRI}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{  No notes for this function either }

\examples{
    # load an example data set
    extdata.path <- system.file("extdata", package = "SIMAT")
    
    # read RItable from file
    RItable <- getRIStandard(file.name = "RIStandards.csv", path = extdata.path)
}