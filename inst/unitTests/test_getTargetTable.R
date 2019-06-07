## function to perform unit test for getTargetTable

test_getTargetTable <- function() {
    
    checkException(getTargetTable(path = getwd()), 
                   "A target table file is required!")  
}