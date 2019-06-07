## function to perform unit test for readMSL

test_readMSL <- function() {
    
    data.path <- system.file("data", package = "SIMAT")
    
    checkException(readMSL(path = data.path), 
                   "Name of the MSL file should be provided!")
        
    checkException(readMSL(file.name = "Targets.MSL", path = data.path), 
                   "No MSL file found in the provided path!")   
}