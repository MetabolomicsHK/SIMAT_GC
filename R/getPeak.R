## This is the function to get the peaks for all runs

getPeak <- function(Run = list(), file.name = character(), Targets = list(), 
                    target.file.name = character(), drt = 10/60, dsc = 14/2, 
                    weight = 2/3, deltaRI = 20, calcRI = NULL, 
                    rt.sort = FALSE) {
    
    ## Initialization
    # load the target info from file, if Targets is not provided
    if (missing(Targets)) {
        # check if target.file is provided
        if (missing(target.file.name)) {
            stop("When \"Target\" is not provided, \"target.file.name\" is required!")
        }
        Targets <- readRDS(target.file.name)    
    }
        
    compound <- Targets$compound
    rt <- Targets$rt
    ms <- Targets$ms
    sp <- Targets$sp
    ri <- Targets$ri
    quantFrag <- Targets$quantFrag
    num.compound <- length(compound)
            
    # sort based on RTs, if asked by user
    if (rt.sort) {
        rt <- sort.int(rt, index.return = TRUE)$x
        ind <- sort.int(rt, index.return = TRUE)$ix
        ms <- ms[ind]
        sp <- sp[ind]
        ri <- ri[ind]
        quantFrag <- quantFrag[ind]
        compound <- compound[ind]
    }
    
    # sort fragments based on quantifier mass
    for (j in 1:num.compound) {
        num.frags <- length(ms[[j]])
        indFrag <- seq(1, num.frags)    
        indFrag <- c(quantFrag[j], indFrag[indFrag != quantFrag[j]])
        ms[[j]] <- ms[[j]][indFrag]
        sp[[j]] <- sp[[j]][indFrag]
    }
    
    
    ## get EIC data of each run per target

    # check if a single run is provided, if not, the file.name should be 
    # provided which includes all the file names of the runs to be processed, 
    # otherwise the peaks of a single run provided by Run variable is retreived
    if (missing(Run)) {
        # get the number of runs
        if (missing(file.name)) {
            stop("When \"Run\" is not provided, the \"file.name\" is required!")    
        }            
        num.runs <- length(file.name) 
        
        # initialize the output variable
        dsPeaks <- list()
      
        for (i in 1:num.runs) {
            # load related RData file for each run
            Run <- readRDS(file = paste(file.name[i], '.rds', sep = ""))
        
            # call getEIC to find the EIC of each target
            for (j in 1:num.compound) {
                peakEIC <- getEIC(Run = Run, compound = compound[j], 
                                  ms0 = ms[[j]], sp0 = sp[[j]], rt0 = rt[j], 
                                  drt = drt, dsc = dsc, ri0 = ri[j], 
                                  weight = weight, deltaRI = deltaRI,
                                  calcRI = calcRI)
                runPeaks[j] <- list(peakEIC)
            }
        
        dsPeaks[i] <- list(runPeaks)
        
        }

    } else {
        # call getEIC to find the EIC of each target
        runPeaks <- list()
        
        for (j in 1:num.compound) {
            peakEIC <- getEIC(Run = Run, compound = compound[j], 
                              ms0 = ms[[j]], sp0 = sp[[j]], rt0 = rt[j], 
                              drt = drt, dsc = dsc, ri0 = ri[j], 
                              weight = weight, deltaRI = deltaRI, 
                              calcRI = calcRI)
            runPeaks[j] <- list(peakEIC)
        }
        
        dsPeaks <- runPeaks
    }

    ## output the results 
    # saveRDS(dsPeaks, file = "dsPeaks.rds", compress = "xz")
    return(dsPeaks)
    
}