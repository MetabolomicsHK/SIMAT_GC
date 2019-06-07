## function to perform unit test for optFrag

test_optFrag <- function() {
    
    data(Library)
    
    checkException(optFrag(Library = Library), 
                   "Either Targets or target.table should be provided!")
    
    data(Targets)
    
    checkException(optFrag(Targets = Targets), 
                    "A library should be provided for optimization!")
}