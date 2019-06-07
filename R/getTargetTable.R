## function to get the targets table information from user
# this function reads the targets info including the names of the compounds
# together with the the mass of selected fragments, the names should be clear
# enough to be searched by getTarget function when required

getTargetTable <- function(target.table.file.name = character(), 
                           path = getwd()) {
    
    # check if the target table file is provided
    if (missing(target.table.file.name)) {
        stop("A target table file is required!")
    }
    
    # define a useful function for trimming strings
    trim <- function(s = character()) {
        s <- gsub("\t", " ", s)
        s <- gsub("\"", "", s)
        s <- gsub("^\\s+|\\s+$", "", s)
        return(s)
    }
        
    # initialization
    compound <- character(); ms <- list(); numFrag <- numeric()
    
    # get the current path
    path.current <- getwd()
    setwd(path)
    
    # read the info from file line by line
    Lines <- readLines(target.table.file.name)
    # Lines <- c(Lines, "")
    
    # retain original path
    setwd(path.current)
    
    # get the total number of rows
    L <- length(Lines)
    # index for compounds
    j <- 0
    
    for (i in 1:L) {
        # trim the leading and trailing blank characters
        Line <- trim(Lines[i])
        
        if (nchar(Line)) {
            
            j <- j + 1
            
            ind.name <- gregexpr(pattern = "NAME", text = toupper(Line))[[1]][1]
            ind.mass <- gregexpr(pattern = "MASS", text = toupper(Line))[[1]][1]
            ind.numFrag <- gregexpr(pattern = "NUMFRAG", 
                                                text = toupper(Line))[[1]][1]
            
            # extract the compound name
            ind.name.start = ind.name + 4
            
            if (ind.mass == -1) {
                if (ind.numFrag == -1) {
                    ind.name.end <- nchar(Line)
                } else {
                    ind.name.end <- ind.numFrag - 1
                }
            } else {
                ind.name.end <- ind.mass - 1
            }
            
            compound <- c(compound, trim(substr(Line, ind.name.start, 
                                                                ind.name.end)))
            
            # extract the compound selected mass                
            if (ind.mass == -1) {
                ms[[j]] <- numeric()
            } else {
                ind.mass.start <- ind.mass + 4
                
                if (ind.numFrag == -1) {
                    ind.mass.end <- nchar(Line)
                } else {
                    ind.mass.end <- ind.numFrag - 1
                }
                
                ms[[j]] <- as.numeric(strsplit(trim(substr(Line, ind.mass.start, 
                                                    ind.mass.end)), " ")[[1]])
            }            
            
            # extract the number of fragments
            if (ind.numFrag == -1) {
                numFrag[j] = length(ms[[j]])                
            } else {
                ind.numFrag.start <- ind.numFrag + 7
                ind.numFrag.end <- nchar(Line)
            
                numFrag[j] <- as.numeric(strsplit(trim(substr(Line, 
                                    ind.numFrag + 7, nchar(Line))), " ")[[1]])
            }         
        }     
    }
        
        
    # ouput the results as a list
    target.table <- list(compound = compound, ms = ms, numFrag = numFrag)
    
        
    return(target.table)
}