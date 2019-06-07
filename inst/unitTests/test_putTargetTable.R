## function to perform unit test for putTargetTable

test_putTargetTable <- function() {
    
    Test <- "Test"
    checkException(putTargetTable(target.table = Test), 
                   "target.table should be a list")    
    
    Table <- list()
    checkException(putTargetTable(target.table.file.name = Table), 
                   "Please provide a string for the output file name!")    
}