##  This function reads NIST mass spectral library (MSL) files
#   As MSL files are not in table format, we need to read them line by line

readMSL <- function(file.name = character(), path = getwd(), Save = FALSE) {
    
    ## check if input minimum requirements are satisfied
    if (missing(file.name)) {
        stop("Name of the MSL file should be provided!")
    }
    
    file.name <- file.path(path, file.name)

    if (!length(file.name)) {
        stop("No MSL file found in the provided path!")
    }
    
    ## define a useful function for trimming strings
    trim <- function(s = character()) {        
        s <- gsub("\t", " ", s)
        s <- gsub("\"", "", s)
        s <- gsub("^\\s+|\\s+$", "", s)
        return(s)        
    }
    
    ## initialization
    ms <- list(); sp <- list(); ri <- numeric(); rt <- numeric()
    compound <- character(); spec <- 0
    
    # path processing
    path.current <- getwd()
    setwd(path)
    
    # read the target file line by line
    Lines <- readLines(file.name)
    #Lines <- c(Lines, "")
    # get the number of rows
    L <- length(Lines)
    
    
    ## extract the required data
    # index for compounds
    j <- 0
        
    for (i in 1:L) {
        # trim the leading and trailing blank characters
        Line <- trim(Lines[i])
            
        if (substr(Line, 1, nchar("NAME:")) == "NAME:") {
            compound <- c(compound, iconv(trim(gsub("NAME:", "", Line)), 
                                          "latin1", "ASCII", sub  = " "))
            spec.flag <- 0
        } 
        else if (substr(Line, 1, nchar("RI:")) == "RI:") {
            ri <- c(ri, as.numeric(trim(gsub("RI:", "", Line))))
            spec.flag <- 0
        } 
        else if (substr(Line, 1, nchar("RT:")) == "RT:") {
            rt <- c(rt, as.numeric(trim(gsub("RT:", "", Line))))
            spec.flag <- 0
        }
        else if (substr(Line, 1, 1) == "(") {
            if (!spec.flag) { # start getting a new spectrum
                spec.flag <- 1
                j <- j + 1
                spec <- trim(gsub("[()]", "", Line))
                spec <- as.numeric(strsplit(spec, " ")[[1]])
                spec <- spec[!is.na(spec)]
            }
            else { # or get the additional rows of the spectrum
                addSpec <- trim(gsub("[()]", "", Line))
                addSpec <- as.numeric(strsplit(addSpec, " ")[[1]])
                addSpec <- addSpec[!is.na(addSpec)]
                spec <- c(spec, addSpec)
            }
        }
        else {
            spec.flag <- 0
        }
        # extract mass and intensity of fragments
        if (!spec.flag & spec[1]) {
            ms[[j]] <- spec[seq(1, length(spec), 2)]
            sp[[j]] <- spec[seq(2, length(spec), 2)]
            spec <- 0
        }
    }
    
    
    ## collecting info for the output
    Targets <- list(rt = rt, ri = ri)
    Targets$compound <- compound
    Targets$ms <- ms
    Targets$sp <- sp
    
    
    ## saving and returning the result
    if (Save) {
        setwd(path.current)        
        file.name <- paste(file.name, ".rds", sep = "")
        saveRDS(Targets, file = file.name, compress = "xz")
    } 
        
    return(Targets)
} 