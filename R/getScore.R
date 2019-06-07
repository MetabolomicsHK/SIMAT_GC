## This is the function to calculate the similarity score between two
## compounds using spectral matching and RI similarity

getScore <- function(trueSpec = numeric(), refSpec = numeric(),
                     trueRI = 0, refRI = 0, deltaRI = 30) {
    
    ## check if minimum requirements are satisfied
    if (missing(trueSpec) | missing(refSpec)) {
        stop("Both trueSpec and refSpec should be provided!")
    }
    
    ## initialization
    num.frags <- length(refSpec)
    # wieght of dot product score in the combined spectral matching score
    w <- 1/5
    
    
    ## calculate RI score
    # check if RI values are numeric
    if (is.null(trueRI)) {
        trueRI <- 0
    }
    if (is.null(refRI)) {
        refRI <- 0
    }
    
    # get RI score
    if (trueRI == 0 & refRI == 0) {
        RIscore <- 1
    } else {
        RIscore <- exp(-abs(trueRI - refRI) / deltaRI)
    }
    
    ## calculate spectral matching score
    #  weighted dot product score
    D <- sqrt(sum(refSpec * trueSpec) / sqrt(sum(refSpec^2) * sum(trueSpec^2)))
    
    #  score based on pairwise ratios of fragments
    R <- 0
    
    for (i in 1:(num.frags-1)) {
        for (j in (i+1):num.frags) {
            
            r <- (refSpec[i]/refSpec[j]) * (trueSpec[j]/trueSpec[i])
            
            if (is.na(r)) {
                r <- 1
            } 
            
            R <- R + min(r, 1/r)
        }        
    }
    
    R <- R / (num.frags * (num.frags - 1) / 2)
    
    #  combined spectral matching score
    SMscore <- w * D + (1-w) * R
    
    ## combined score
    Score <- (RIscore * SMscore^2)^(1/3)
    
    # check if score is not NAN or NA
    if (is.na(Score) | is.nan(Score)) {
        Score <- 0
    }
        
    
    return(Score)
}