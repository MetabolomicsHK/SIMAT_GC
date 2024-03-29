%% getPeak function %%
\name{getPeak}
\alias{getPeak}

\title{Get all peaks corresponding to targets in several runs}

\description{This function retrives all the corresponding peaks of targets of interest in several runs.}

\usage{
getPeak(Run = list(), file.name = character(), Targets = list(), 
            target.file.name = character(), drt = 10/60, dsc = 14/2, 
                weight = 2/3, deltaRI = 20, calcRI = NULL, rt.sort = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Run}{a list including a single run information, if provided, the next argument, i.e. file.name, is ignored.}
    
    \item{file.name}{a character vector of file names, i.e. the run file names generated by \code{readCDf}.}
    
    \item{Targets}{a list including the Targets information, e.g. acquired by \code{getTarget} function.}
    
    \item{target.file.name}{a character object, i.e. string,  of the name of the file generated by \code{getTarget}.}
    
    \item{drt}{a numeric value of the retention time window width in seconds, optional but recommended}
    
    \item{dsc}{a numeric value of average half peak width based on the TICs, optional but recommended}
    
    \item{weight}{a numeric value in [0,1] interval to calculated a combined weighted similarity scores based on Apex and area under EIC curve.}
    
    \item{deltaRI}{a numeric value for the penalty on the retention index similarity score.}
    
    \item{calcRI}{a function name to calculate RI based on RI calibration information, can be obtained by \code{getRI} function, optional.}
    
    \item{rt.sort}{a boolean value to sort the targets based on retention time before performing peak detection, optional}
}

\details{This function performs peak detection for a list of targets in several runs by calling \code{getEIC}. The list of targets should include the information on retention times, retention indexes, mass and intensity of the fragments. It can also include the name of the targets, i.e. compounds.
}

\value{If multiple runs are provided, a list of list objects, one for each run. If a single run is provided, a list containing peaks information, i.e. the same as \code{getEIC} output.}

\seealso{\code{\link{getEIC}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

%\note{  No notes for this function either }

\examples{
    # load an RData file including a single run data acquired by readCDF
    data("Run")
    
    # load targets information
    data(Targets)
    
    # get all the corresponding peaks of the target list
    runPeaks <- getPeak(Run = Run, Targets = Targets)
}