## function to perform unit test for getRI

test_getRI <- function() {
    
    data(RItable)
    
    calcRI <- getRI(RItable = RItable)
    
    checkEquals(class(calcRI), "function")
    checkEquals(class(calcRI(15.0)), "numeric")
    checkEqualsNumeric(calcRI(10.0), 953.2374)
}