## function to perform unit test for getPeak

test_getPeak <- function() {
    
    data(Targets)
    checkException(getPeak(Targets = Targets, weight = 2/3, deltaRI = 20), 
                   "When \"Run\" is not provided, the \"file.name\" is 
                   required!")
    
    data(Run)    
    checkException(getPeak(Run = Run, drt = 10/60, weight = 2/3, deltaRI = 20), 
                   "When \"Target\" is not provided, \"target.file.name\" is 
                   required!")
}