%% optFrag function %%
\name{optFrag}
\alias{optFrag}

\title{Fragment selection through optimization}

\description{This function can be used to select the quantifier fragments through optimization. The optimization criteria are less overlapping targets or compounds with higher intensities.
}

\usage{
optFrag(Targets = list(), Library = list(), target.table = list(), 
            deltaRI = 20, deltaRT = 4, numFrag.default = 4, forceOpt = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Targets}{a list of targets and their information including retention time and index, mass and intensity of the fragments extracted from a target list}
    
    \item{Library}{a list of compounds and their information including retention time and index, mass and intensity of the fragments extracted from an .MSL library.}
    
    \item{target.table}{a list including the name of the compounds, this is required when a library is provided to extract the targets information from the library. This value can be obtained using \code{getTargetTable} function.}

    \item{deltaRI}{a numeric value for the penalty on the retention index similarity score}

    \item{deltaRT}{a numeric value for the penalty on the retention time similarity score}    
    
    \item{numFrag.default}{a numeric value, where neither the number of fragments, i.e. \code{numFrag}, nor the mass of suggested fragments are not provided, the algorithm chooses \code{numFrag.default} fragments from the library.}
    
    \item{forceOpt}{a logical value to force the optimization where the default is \code{FALSE}. When it is set to TRUE, even when fragment masses are provided by user, the algorithm picks the omptimized fragments. The number of selected fragments is then decided by the number of suggested fragments or \code{numFrag.default}.}
}

\details{This function is called in \code{getTarget} function to optimize the selection of the fragments where fragments with higher intensity and less overlapping profile with neigbour compounds are preferred. Therefore, it is required that the user provides a library, e.g. stablished, in-house, or a combination of both. It is important that the library does not include any targets. The function sorts the fragments based on their intensities and checks if there are any overlapping compounds, e.g. based on retention time or retention index, which shares the same fragment, i.e. the same mass in the spectra. By penalizing the shared fragments based on their retention time or index difference with overlapping compounds, and also considering the intensity of the fragments, fragments are priorotized. As a result the quantifier mass can be selected from the top. It is recommended to provide the RI information, but if  it is not available, the code uses retention times for this purpose.
}

\value{list}

\seealso{\code{\link{getTarget}}, \code{\link{getTargetTable}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{Important note: the library should not include any compounds in the target list}

\examples{
    # load an RData file including a single run data acquired by readCDF
    data("Run")
    
    # load the target table information
    data(target.table)

    # load the background library to be used with fragment selection
    data(Library)
    
    # forcing optimization to get the targets info
    optTargets <- optFrag(Library = Library, target.table = target.table, 
                                                            forceOpt = TRUE)
}