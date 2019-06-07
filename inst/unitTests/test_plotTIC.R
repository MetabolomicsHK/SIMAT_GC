## function to perform unit test for plotTIC

test_plotTIC <- function() {
    
    checkException(plotTIC(), "Either Run or file.name should be provided!")    
}