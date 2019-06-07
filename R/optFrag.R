## This function gets a list of targets and list of library items and generates
## a list of optimized fragments for each target to be used in a SIM analysis

optFrag <- function(Targets = list(), Library = list(), target.table = list(), 
                    deltaRI = 20, deltaRT = 4, numFrag.default = 4, 
                    forceOpt = FALSE) {
    
    
    # function to avoide imperfect compound names issues
    compStr <- function(x = character()) {gsub("[^a-z^0-9]", "", tolower(x))}
        
    # penalty function
    penFunc <- function(x1, x2, dx) {max(0, 1 - ((x1-x2)/dx)^2)}

    
    ## initializatoin
    sortedFrag <- list(); quantFrag <- numeric()
    # mass and intensity of the fragments for the ouput list
    Ms <- list(); Sp <- list()
        
    
    ## check if either Targets or target.name is provided
    if (missing(Targets) & missing(target.table)) {
        stop("Either Targets or target.table should be provided!")
    } 
    else if (missing(Library)) {
        stop("A library should be provided for optimization!")
    }
    
    
    ## if Targets is provided
    if (missing(Targets)) {
        Targets <- list()
    } else {
        # convert targets to a target table
        target.table <- list(compound = Targets$compound, ms = Targets$ms)
        
        # merge targets with library into a combined library
        # by removing duplicated compounds
        Lib <- Library; Library <- list()
        L1 <- length(Targets$compound); L2 <- length(Lib$compound)
                     
        for (i in 1:L1) {
            Library$rt[i] <- Targets$rt[i]
            Library$ri[i] <- Targets$ri[i]
            Library$compound[i] <- Targets$compound[i]
            Library$ms[i] <- Targets$ms[i]
            Library$sp[i] <- Targets$sp[i]
        }

        for (i in 1:L2) {
            Library$rt[i+L1] <- Lib$rt[i]
            Library$ri[i+L1] <- Lib$ri[i]
            Library$compound[i+L1] <- Lib$compound[i]
            Library$ms[i+L1] <- Lib$ms[i]
            Library$sp[i+L1] <- Lib$sp[i]            
        }
        
        Library <- unique(mapply(c, Targets, Library, SIMPLIFY = FALSE))
    }
    

    ## use target.table, from input argument or generated from target list

    L <- length(target.table$compound)
        
    RI <- Library$ri
    RT <- Library$rt
    MS <- Library$ms
    SP <- Library$sp
    LM <- length(MS)
    
    
    ## to overcome issues with imperfect match of the names
    libComp <- compStr(Library$compound)
    targComp <- compStr(target.table$compound)
    
    
    ##  retrieving the target info and optimization (if desired)
    for (i in 1:L) {
            
        # find the target index in the library
        ind <- match(target.table$compound[i], Library$compound)
        
        # check if the compound found, if not, try an imperfect match
        if(is.na(ind)) {
            ind <- match(targComp[i], libComp)
        }
        
        # if the compound is still not found in the library
        if(is.na(ind)) {
            # give user a warning message
            warning(paste("Compound number", as.character(i), "named", 
                          target.table$compound[i], "not found in the library", 
                            sep = " "), call. = FALSE)
            
            Targets$rt[i] <- max(Targets$rt[i], 0)
            Targets$ri[i] <- max(Targets$ri[i], 0)
            
            Targets$compound[i] <- target.table$compound[i]
                
            Targets$ms[i] <- target.table$ms[i]
            
            if (is.null(unlist(Targets$sp[i]))) {
                Targets$sp[[i]] <- rep(0, length(target.table$ms[[i]]))
            }
            
            quantFrag[i] <- 1
            sortedFrag[[i]] <- 1:length(target.table$ms[[i]])
            
            # skip the ith iteraton
            next
        }
        
        # length of the fragments from library
        Lm <- length(MS[[ind]])
            
        # penalty for fragments of ith target
        Pen <- numeric()
        
        # RI and RT of the target
        Targets$rt[i] <- rt <- RT[ind]
        Targets$ri[i] <- ri <- RI[ind]
        Targets$compound[i] <- target.table$compound[i]
        
        for (j in 1:Lm) {
            # get the mass and intensity of the j-th fragment
            ms <- MS[[ind]][j]
            sp <- SP[[ind]][j]
                
            # index of the overlapping analytes with a fragment of the same mass
            pen <- numeric()
                 
            # if retention index is provided
            if (ri) {
                for (k in 1:(LM-1)) {
                    if (sum(ms == MS[-ind][[k]])) {
                        pen <- c(pen, penFunc(ri, RI[-ind][[k]], deltaRI))
                    }
                }
            } 
            # if retention index is not provided
            else {
                for (k in 1:(LM-1)) {
                    if (sum(ms == MS[-ind][[k]])) {
                        pen <- c(pen, penFunc(rt, RT[-ind][[k]], deltaRT))
                    }
                }                
            }        
            
            Pen[j] <- sum(pen)/sp
        }
            
        # sort fragments based on their penalty scores
        #sortFrag <- c(sortFrag, sort.int(Pen, index.return = TRUE)$ix)
        #quantFrag[i] <- which.min(Pen)       
        sortedFrag[i] <- list(order(Pen, -SP[[ind]]))
        
        # see if user has already suggested fragments
        if (length(target.table$ms[[i]]) & !forceOpt) {
            
            # get the mass of suggested fragments
            Ms[[i]] <- target.table$ms[[i]]
        
            # initialize the intensity
            Sp[[i]] <- numeric()
                    
            for (j in 1:Lm) {
                # get the mass of the jth fragment in the ith target
                ms <- Ms[[i]][j]
                # find the index of the related fragment in the library
                ind.ms <- match(ms, MS[[ind]])
                # retrieve the intensity of the related fragment from library
                if (!is.na(ind.ms)) {
                    Sp[[i]] <- c(Sp[[i]], SP[[ind]][ind.ms])
                }
                # set the quantFrag, pointing to the first suggested fragment
                quantFrag[i] <- 1
            }
        }
        # if user has not suggested mass fragments
        else {
            # check if the number of desired fragments (numFrag) is provided or 
            # the default value should be used
            numFrag <- max(target.table$numFrag[i], numFrag.default)        
                
            # select related fragments based on numFrag
            Ms[[i]] <- MS[[ind]][sortedFrag[[i]][1:numFrag]]
            Sp[[i]] <- SP[[ind]][sortedFrag[[i]][1:numFrag]]
            # set the quantFrag to the one with least penalty
            quantFrag[i] <- 1
            #quantFrag[i] <- sortedFrag[[i]][1]                    
        }
        
        # set mass and intensity of the fragments for the target
        Targets$ms[i] <- Ms[i]
        Targets$sp[i] <- Sp[i]
    }
    
    
    ## output variable
    Targets$quantFrag <- quantFrag
    Targets$sortedFrag <- sortedFrag
    
    return(Targets)
}