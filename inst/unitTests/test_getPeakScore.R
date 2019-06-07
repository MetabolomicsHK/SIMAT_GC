## function to perform unit test for getPeak

test_getPeakScore <- function() {
    
    checkException(getPeakScore(deltaRI = 20), paste("Either as single run, i.e.
                    runPeaks, or multiple runs,","i.e. dsPeaks, should be 
                                                     provided", sep = ""))    
}