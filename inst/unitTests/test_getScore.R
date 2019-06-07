## function to perform unit test for getScore

test_getScore <- function() {
    
    checkException(getScore(deltaRI = 20), 
                   "Both trueSpec and refSpec should be provided!")
    
    trueSpec = c(100, 200, 50)
    refSpec = c(100, 200, 50)
    
    checkEqualsNumeric(getScore(trueSpec = trueSpec, refSpec = refSpec), 1)
    
    refSpec = c(90, 210, 55)
    
    checkEqualsNumeric(getScore(trueSpec = trueSpec, refSpec = refSpec), 0.9329,
                       tolerance = 0.0001)
}