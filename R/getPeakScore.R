## This is the function to calculate the similarity score for all peaks

getPeakScore <- function(runPeaks = list(), dsPeaks = list(), 
                         deltaRI = 20, weight = 2/3, plot = FALSE) {
    
    ## check if a single run is provided
    if (!missing(runPeaks)) {
        # initialization
        num.compound <- length(runPeaks)
        
        Scores <- numeric() 
        
        # get the scores for each target            
        for (j in 1:num.compound) {
            
            spApex <- runPeaks[[j]]$intApex
            spArea <- runPeaks[[j]]$area
            sp <- runPeaks[[j]]$sp
            ri <- runPeaks[[j]]$ri
            ri0 <- runPeaks[[j]]$ri0
                            
            ScoreApex <- getScore(trueSpec = spApex, refSpec = sp,
                                  trueRI = ri, refRI = ri0, deltaRI = deltaRI)
            ScoreArea <- getScore(trueSpec = spArea, refSpec = sp,
                                  trueRI = ri, refRI = ri0, deltaRI = deltaRI)
            
            Scores[j] <- weight * ScoreApex + (1-weight) * ScoreArea
        }                
        
    }
    # if a single run is not provided
    else if (missing(dsPeaks)) {
        stop(paste("Either as single run, i.e. runPeaks, or multiple runs,",
                "i.e. dsPeaks, should be provided", sep = ""))
    } else {
        # Initialization
        num.runs <- length(dsPeaks)
        num.compound <- length(dsPeaks[[1]])
        
        Scores <- matrix(, nrow = num.compound, ncol = num.runs)
    
        # get the scores for each target per run
        for (i in 1:num.runs) {        
        
            for (j in 1:num.compound) {
                
                spApex <- dsPeaks[[i]][[j]]$intApex
                spArea <- dsPeaks[[i]][[j]]$area
                sp <- dsPeaks[[i]][[j]]$sp
                ri <- dsPeaks[[i]][[j]]$ri
                ri0 <- dsPeaks[[i]][[j]]$ri0
                            
                ScoreApex <- getScore(trueSpec = spApex, refSpec = sp,
                                trueRI = ri, refRI = ri0, deltaRI = deltaRI)
                ScoreArea <- getScore(trueSpec = spArea, refSpec = sp,
                                trueRI = ri, refRI = ri0, deltaRI = deltaRI)
            
                Scores[j, i] <- weight * ScoreApex + (1-weight) * ScoreArea
            }        
        }    
    }
    
    ## plot histogram of the scores
    if (plot) {
        scorePlot <- ggplot(melt(Scores, value.name = "Scores"), 
                            aes(x = Scores)) + geom_histogram(binwidth = 0.02, 
                            color = "darkblue", fill = "blue") + 
                            scale_x_continuous(limits=c(0,1))
    
        print(scorePlot)
    }
    
    # return the output, e.g. the scores
    return(Scores)    
}