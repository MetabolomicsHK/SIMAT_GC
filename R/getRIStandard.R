## This function gets the measured retention times and retention indecies
#  of the RI standards to be used for retention time calibration
#  The input is a csv file with the one column (RT) and one column o

getRIStandard <- function(file.name = character(), path = getwd()) {
    
    # set path
    path.current <- getwd()
    setwd(path)
    
    # read file
    if (missing(file.name)) {
        stop("A file name should be provided!")
    }
    
    dat <- read.csv(file = file.name, header = TRUE)
    dat.names <- tolower(names(dat))
    
    # generate ouput
    rt <- dat[, match( "rt", dat.names)]
    ri <- dat[, match( "ri", dat.names)]
    RItable <- data.frame(rt = rt, ri = ri)
    
    # set path
    setwd(path.current)
    
    return(RItable)
}