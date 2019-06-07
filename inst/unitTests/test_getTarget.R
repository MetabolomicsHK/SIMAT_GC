## function to perform unit test for getTarget

test_getTarget <- function() {
    
    checkException(getTarget(Method = "target"), 
                   "When Method = \"target\", a target file should be provided")
        
    checkException(getTarget(Method = "library"), 
                   paste("When Method = \"library\","
                    , "a library file and a target.table should be provided!"))
    
    checkException(getTarget(Method = "combin"), 
                   paste("When Method = \"combined\", a target file and a",
                        "library file should be provided!"))   
    
    checkException(getTarget(Method = "optimize"), 
                   paste("Unknown Method optimize", sep = " "))
}