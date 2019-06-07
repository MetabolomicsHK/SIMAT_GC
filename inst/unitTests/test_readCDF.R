## function to perform unit test for readCDF

test_readCDF <- function() {
    
    data.path <- system.file("data", package = "SIMAT")
    
    checkException(readCDF(path = data.path), "No CDF files found!")    
}