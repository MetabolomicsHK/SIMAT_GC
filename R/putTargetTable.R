##  Function to write Target info in  NIST mass spectral library (MSL) format
#   As MSL files are not in table format, we need to write them line by line

putTargetTable <- function(target.table = list(), 
                           target.table.file.name = character()) {
    
    # check if any required argument is missing
    if (missing(target.table) | missing(target.table.file.name)) {
        stop("Either the target table or the file name is missing!")
    } else if (!is.character(target.table.file.name)) {
        stop("Please provide a string for the output file name!")
    } else if (!is.list(target.table)) {
        stop("target.table should be a list")
    }
    
    # initialize
    num.compound <- length(target.table$compound)
    Lines <- character()
    k <- 1
    
    # create output line by line
    for (i in 1:num.compound) {
        
        compound.name <- paste("\"", target.table$compound[i], "\"", sep = "")
            
        Lines[k] <- paste("Name", compound.name, "Mass", sep = "\t")
                            
        for (j in 1:length(target.table$ms[[i]])) {
            Lines[k] <- paste(Lines[k], as.character(target.table$ms[[i]][j]), 
                              sep = "\t")
        }        
                
        k <- k + 1
    }
    
    # write the results to the file
    writeLines(Lines, con = target.table.file.name)    
}