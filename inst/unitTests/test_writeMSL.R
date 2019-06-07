## function to perform unit test for writeMSL

test_writeMSL <- function() {
    
    data(Targets)
    
    checkException(writeMSL(Targets = Targets), 
                   "Either the targets or the file name is missing!")
        
    checkException(writeMSL(target.file.name = "Targets.MSL"), 
                   "Either the targets or the file name is missing!")   
}