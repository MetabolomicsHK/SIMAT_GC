## function to perform unit test for plotEIC

test_plotEIC <- function() {
    
    checkException(plotEIC(fig.name = "Fig1"), "A peak EIC should be provided!")                   
    
    eic <- list()
    checkException(plotEIC(peakEIC = eic), "Empty peak EIC!")
    
    checkException(plotEIC(peakEIC = 1:5), "peakEIC should be a list!")
}