%% plotEIC function %%
\name{plotEIC}
\alias{plotEIC}

\title{Plotting EIC of one peak}

\description{This function can be used to plot the EIC profile of a peak, where the peak has been optianed by \code{getPeak} or \code{getEIC} functions. The plot also shows a pseudo peak, which is an illustratiion based on reference spectrum.
}

\usage{
plotEIC(peakEIC = list(), fig.name = character())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{peakEIC}{a list inclduing the EIC information of a peak}
    
    %\item{Save}{a boolean value flag, if TRUE, the resulted plot is saved as a png file and no figure is shown.}
    
    \item{fig.name}{a character vector (string), if provided, the figure is saved in pdf format with this name.}

    %\item{res}{a numeric value for the resolution of the png file in dpi, default is 300}    
}

\details{This function plots the EIC profile of one peak. The peak information can be obtained using \code{getPeak} or \code{getEIC} finctions. The peak profile includes the measured intensities in the raw data correponding to a certain target in on specific run. The intensities are plotted versus recorded retention time in the sample. Different colors are used for different fragments and the mass of each fragment is included in the legend of the plot. Also, to make the visual comparison between the true and reference spcetra available, a pseudo peak based on the reference spectra is provided in a subplot showing the ratios between fragments. Finally, the expected and actual retention times of the target is shown in gray and red respectively.
}

\value{A logical value which is TRUE if the resulted plot is saved}

\seealso{\code{\link{plotTIC}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    # load an RData file including a single run data acquired by readCDF
    data("Run")
    
    # load targets information
    data(Targets)
    
    # get all the corresponding peaks of the target list
    runPeaks <- getPeak(Run = Run, Targets = Targets)
    
    # plot the EIC of the first target
    plotEIC(runPeaks[[1]])
}