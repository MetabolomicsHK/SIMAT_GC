## This function reads CDF files

readCDF <- function(path = getwd()) {
    
    path.current <- getwd()
    
    file.name <- list.files(path = path, pattern = ".CDF", full.names = FALSE)
    
    if (!length(file.name)) {
        stop("No CDF files found!")
    }
        
    for (i in 1:length(file.name)) {
        
        setwd(path)
        
        CDFdata <- openMSfile(file.name[i], backend = "netCDF") 
        pk <- peaks(CDFdata)
        hd <- header(CDFdata)
        rt <- hd$retentionTime
        sc <- hd$seqNum
        tic <- hd$totIonCurrent
        
        Run <- list(rt = rt, sc = sc, tic = tic)
        Run$pk <- pk
        
        setwd(path.current)
        saveRDS(Run, file = paste(file.name[i], ".rds", sep = ""))
        #save(Run, file=paste(file.name[i], ".rda", sep=""), compress="xz")
    }
    
    # return the file names, can be used with getPeak()
    return(file.name)
}