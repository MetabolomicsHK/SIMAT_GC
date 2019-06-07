%% writeMSL function %%
\name{writeMSL}
\alias{writeMSL}

\title{Write targets information into a file with mass spectral library (MSL) format.}

\description{This funtion gets the targets information and writes the data in the NIST mass spectral library (MSL) format. 
}

\usage{
writeMSL(Targets = list(), target.file.name = character())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Targets}{a list including the Targets information, e.g. acquired by \code{getTarget} function.}
    
    \item{target.file.name}{a character object, i.e. string,  of the name of the output file.}
}

\details{NIST mass spectral library, i.e. MSL, data files include compound names and their spectral information. The spectral information consist of number of fragments for each compound, together with the mass and relative and usually normalized intensity of each fragment. Also, the retention time and retention index of the compounds are provided which helps improve the identification by avoding false positives. This functions is used to write the targets information into a file with MSL format.
}

\value{A logical value}

\seealso{\code{\link{readMSL}}, \code{\link{getTarget}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{no notes for this function}

\examples{
    # load targets information
    data(Targets)
                                                
    # write the targets into a file with MSL format
    writeMSL(Targets = Targets, target.file.name = "myTargets.MSL")
}