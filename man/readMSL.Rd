%% readMSL function %%
\name{readMSL}
\alias{readMSL}

\title{Read mass spectral library (MSL) files}

\description{This funtion read data in NIST mass spectral library (MSL) format and returns a list of compounds, with their names, retention times, retention indexes (if provided), together with mass and intensity of all fragments.
}

\usage{
readMSL(file.name = character(), path = getwd(), Save = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{file.name}{a string including the full name of a .MSL file including the library information.}
    
    \item{path}{a string including the full path to the location of the file.name, optional}
    
    \item{Save}{if TRUE, the function also saves the list as an RDdata object. It always returns the targets as list.}
}

\details{NIST mass spectral library, i.e. MSL, data files include compound names and their spectral information. The spectral information consist of number of fragments for each compound, together with the mass and relative and usually normalized intensity of each fragment. Also, the retention time and retention index of the compounds are provided which helps improve the identification by avoding false positives. This functions usually is used indirectly by calling \code{getTarget} function.
}

\value{A list including target information:
\item{rt}{a numeric vector of retention times}
\item{rt}{a numeric vector of retention indexes}
\item{ms}{a list including vectors of fragment masses of each target}
\item{sp}{a list including vectors of fragment intensities of each target}
\item{compound}{a list including character vectors of the target chemical names}
}

\seealso{\code{\link{writeMSL}}, \code{\link{readCDF}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{no notes for this function}

\examples{
    # load an example data set
    extdata.path <- system.file("extdata", package = "SIMAT")
    
    # get the list of targets in a file with MSL format from Example Data Set
    Targets1 <- readMSL(file.name="Targets_1.MSL", path = extdata.path)
}