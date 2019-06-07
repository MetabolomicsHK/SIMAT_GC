##  Function to write Target info in  NIST mass spectral library (MSL) format
#   As MSL files are not in table format, we need to write them line by line

writeMSL <- function(Targets = list(), target.file.name = character()) {
    
    # check if any required argument is missing
    if (missing(Targets) | missing(target.file.name)) {
        stop("Either the targets or the file name is missing!")
    }
    
    # initialize
    num.compound <- length(Targets$compound)
    Lines <- character()
    k <- 1
    
    # create output line by line
    for (i in 1:num.compound) {
        
        Lines[k] <- paste("NAME:", Targets$compound[i], sep = " ")
        
        Lines[k+1] <- paste("RI:", Targets$ri[i], sep = " ")
        
        Lines[k+2] <- paste("RT:", Targets$rt[i], sep = " ")
        
        Lines[k+3] <- paste("Num Peaks:", as.character(length(Targets$ms[[i]])), 
                         sep = " ")
        
        # create spectrum
        Spec <- character()
        
        for (j in 1:length(Targets$ms[[i]])) {
            Spec <- paste(Spec, "(", as.character(Targets$ms[[i]][j]), 
                          as.character(Targets$sp[[i]][j]), ")", sep = " ")
        }
        
        Lines[k+4] <- Spec
        
        k <- k + 5
    }
    
    # write the results to the file
    writeLines(Lines, con = target.file.name)    
}