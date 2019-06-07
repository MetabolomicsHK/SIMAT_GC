%% getEIC function %%
\name{getEIC}
\alias{getEIC}

\title{Get the EIC of a peak}

\description{This function retrives the EIC of one peak in one run.}

\usage{
getEIC(Run = list(), compound = "Analyte", ms0 = numeric(), sp0 = numeric(),
                   rt0 = numeric(), drt = 10/60, dsc = 10/2, ri0 = 0, 
                   weight = 2/3, deltaRI = 20, calcRI = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    %\item{scans}{a list of peak information for one run including the scans with mass and intensities}
    
    %\item{rt}{a numeric vector of retention time of all scans of one run}
    
    \item{Run}{a list containing the information of one run obtained by \code{readCDf} funciton.}
    
    \item{compound}{a character vector for the name of the target.}
    
    \item{ms0}{a numeric vector of mass of fragments of the target.}
    
    \item{sp0}{a numeric vector of intensities of fragments of the target.}
    
    \item{rt0}{a numeric value of the expected retention time of the target.}
    
    \item{drt}{a numeric value of the retention time window width in seconds, optional but recommended}
    
    \item{dsc}{a numeric value of average half peak width based on the TICs, optional but recommended}
    
    \item{ri0}{a numeric value of the retention index of the target from library, optional but recommended}
    
    \item{deltaRI}{a numeric value for the penalty on the retention index similarity score}
    
    \item{weight}{a numeric value in [0,1] interval to calculated a combined weighted similarity scores based on Apex and area under EIC curve}

    \item{calcRI}{a function name to calculate RI based on RI calibration information, can be obtained by \code{getRI} function, optional.}
}

\details{This function accepts several parameters including the expected retention, the mass and intensity of several fragments, the retention index of the peak, the extracted data from a raw netCDF file in peak table format, a certain range for searching the retention time, i.e. the retention time window, average half peak width. The function uses a similarity score based on a combined measure from spectral matching and RI similarity, if RI is available, and finds the most appropriate peak in considering the target information.
}

\value{A list containing the peak information:
\item{rtApex}{a numeric value of retention time of the apex based on the quantifier fragment}
\item{intApex}{a numeric value of the intensity of the quantifier fragment at its apex}
\item{RI}{a numeric value of retention index related to the \code{rtApex}}
\item{area}{area under EIC for all fragments of the related target}
\item{EIC}{intensity of EIC profile of all fragments}
\item{RT}{retention times of EIC profiles}
\item{ms}{mass of fragments}
\item{sp}{intensity of the fragments based on the reference spectrum}
\item{rt0}{a numeric value of expected retention time}
\item{compound}{a character vector cintaining the name of the target.}
}

\seealso{\code{\link{getPeak}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    # load an RData file including a single run data acquired by readCDF
    data("Run")
    
    # load targets information
    data(Targets)

    # get the corresponding peak of a target
    peakEIC <- getEIC(Run = Run, compound = Targets$compound[1], 
                    ms0 = Targets$ms[[1]], sp0 = Targets$sp[[1]], 
                    rt0 = Targets$rt[1], ri0 = Targets$ri[1])
}