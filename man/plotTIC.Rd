%% plotTIC function %%
\name{plotTIC}
\alias{plotTIC}

\title{Plotting TIC of one run}

\description{This function can be used to plot the total ion chromatogram (TIC) profile of a run, showing the accumulated measurements for all fragments versus time. The run can be  optianed by \code{getPeak}.
}

\usage{
plotTIC(Run = list(), file.name = character())
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Run}{a list inclduing the information of all detected peaks in one run.}
    
    \item{file.name}{a character vector (string) of names of the runs which is used to name the figure file when saving, if the \code{Run} is provided, this argument is ignored.}
}

\details{This function plots the TIC profile of one run. The run information can be obtained using \code{getPeak} finction. The TIC is the sum of the intensities from all masses at each scan. If argument \code{Run} is provided, the funtion plots the TIC for that run. If a list of file names is provided, the function saves a corresponding plot for each run with the related name.
}

\value{A logical value which is TRUE if the resulted plot is saved}

\seealso{\code{\link{plotEIC}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    # load an RData file including a single run data acquired by readCDF
    data("Run")
    
    # plot TIC of the run
    plotTIC(Run = Run)
}