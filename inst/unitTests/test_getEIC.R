## function to perform unit test for getEIC

test_getEIC <- function() {
    
    checkException(getEIC(compound = "Analyte", ms0 = c(100, 150, 200), 
                       sp0 = c(200, 50, 500), rt0 = 15.0), 
                   "One of the required inputs is missing!")
}