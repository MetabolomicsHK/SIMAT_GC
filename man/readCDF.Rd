\name{readCDF}
\alias{readCDF}

\title{Read raw mass spectrometry data in netCDF files}

\description{
This function read raw netCDF files and provides a list of peaks and retention time of scans.
}

\usage{
    readCDF(path = getwd())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{path}{a string including the full path of the CDF files, optional.}
}

\details{
This function gets a path which is optional. In the path, it searches for CDF files and reads them one by one. An RData file is created for each file separately, keeping the original file names. Each file includes a list of peaks, i.e. a  combination of mass and intensities of scans, and the retention times of the scans.
}

\value{a character vector inlcuding the names of the imported netCDF files. The output then can be used with \code{getPeak} function.}

\seealso{\code{\link{readMSL}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{no notes for this function}

\examples{
    # load an example data set
    extdata.path <- system.file("extdata", package = "SIMAT")
    
    # read CDF files
    file.name <- readCDF(path = extdata.path)
}