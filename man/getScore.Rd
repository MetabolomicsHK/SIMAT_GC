%% getScore function %%
\name{getScore}
\alias{getScore}

\title{Calculate similarity score for a peak}

\description{This function calculates and output the similarity score for a peak while a reference spectrum is provided. The score is based on spectral matching and RI difference between the true peak and the reference.}

\usage{
getScore(trueSpec = numeric(), refSpec = numeric(), 
                        trueRI = 0, refRI = 0, deltaRI = 30)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{trueSpec}{a numeric vector of intensity values for the measure, i.e. true, spectrum.}
    
    \item{refSpec}{a numeric vector of the intensity values for the reference spectrum.}

    \item{trueRI}{a numeric value of the measured, i.e. true, retention index}
    
    \item{refRI}{a numeric value of the reference retention index}
    
    \item{deltaRI}{a numeric value for the penalty on the retention index similarity score}
}

\details{By calling getScore(), this function calculates the similarity score for multiple peaks in multiple runs. This is performed by using spectral and retention index information together.
}

\value{numeric}

\seealso{\code{\link{getPeakScore}}}

\references{\url{http://omics.georgetown.edu/SIMAT.html}}

\author{Mo R. Nezami Ranjbar}

\examples{
    getScore(trueSpec = c(97, 995, 278, 343), refSpec = c(100, 1000, 250, 390))
}