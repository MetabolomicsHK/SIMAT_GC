## This function gets a peak information and plots the EIC profile of fragments

plotEIC <- function(peakEIC = list(), fig.name = character()) {
    
    ## check if the input satisfies the minimum requirements
    if (missing(peakEIC)) {
        stop("A peak EIC should be provided!")
    } else if (!length(peakEIC)) {
        stop("Empty peak EIC!")
    } else if (!is.list(peakEIC)) {
        stop("peakEIC should be a list!")
    }
    
    
    ## get peak EIC info
    eic <- peakEIC$EIC
    rt <- peakEIC$RT/60
    ms <- peakEIC$ms
    sp <- peakEIC$sp
    rtApex <- peakEIC$rtApex
    rt0 <- peakEIC$rt0
    maxIntApex <- max(peakEIC$intApex)
    
    
    ## initialization
    # to get rid of the 'no visible binding for global variable' notes.
    Intensity <- Fragment <- NULL
        
    
    ## create the main EIC plot
    df <- data.frame(t(eic))
    Spc <- stack(df)
    Spc$Time <- rep(rt, dim(eic)[1], dim(eic)[2])
    colnames(Spc)[1:2] <- c("Intensity", "Fragment")
    Spc[, 2] <- rep(as.character(ms), each = length(rt))
    mainPlot <- ggplot(data = Spc, aes(x = Time, y = Intensity, color = Fragment)) +
                    geom_line(size = 1.2) + theme_bw() +
                    ggtitle(peakEIC$compound) +
                    theme(plot.title = element_text( size=20)) +
                    theme(plot.title = element_text(vjust=2)) +                            
                    geom_vline(xintercept = rtApex, color = "red", 
                               linetype = "dashed", size = 0.75) +
                    geom_vline(xintercept = rt0, color = "gray", 
                               linetype = "dashed", size = 0.75)
    
    if (rtApex >= rt0) {
        mainPlot <- mainPlot + theme(legend.position = c(0.9, 0.875), 
                                     aspect.ratio = 1)
    } else {
        mainPlot <- mainPlot + theme(legend.position = c(0.075, 0.875), 
                                     aspect.ratio = 1)
    }
    
    # check if the max ylim is much larger than the max peak apex
    if (ggplot_build(mainPlot)$panel$ranges[[1]]$y.range[2] > 2*maxIntApex) {
        mainPlot <- mainPlot + coord_cartesian(ylim = c(0, 1.5*maxIntApex))
    }
    
    
    ## profile pattern for the pseudo peak based on the reference spectrum
    Time <- seq(-3, 3, 0.1)
    G <- dnorm(Time)
    G <- G / max(G)
    
    num.frag <- length(sp)
    spc <- matrix(, nrow = num.frag, ncol = length(Time))
    
    for (i in 1:num.frag) {        
        spc[i, ] <- G * sp[i]        
    }
    
    df <- data.frame(t(spc))
    pSpc <- stack(df)
    pSpc$Time <- rep(Time, dim(spc)[1], dim(spc)[2])
    colnames(pSpc)[1:2] <- c("Intensity", "Fragment")
    pSpc[, 2] <- rep(as.character(ms), each = length(Time))
    subPlot <- ggplot(data = pSpc, aes(x = Time, y = Intensity, color = Fragment)) +
                        geom_line(size = 0.5) + theme_bw() +
                        theme(legend.position = "none", aspect.ratio = 1,
                                axis.title = element_blank(), 
                                axis.text = element_blank(),
                                axis.ticks = element_blank())
    
    
    # create the viewport for the subplot inside the mainplot
    if (rtApex >= rt0) {
        vp <- viewport(just = c("left", "top"), width = 0.25, height = 0.25, 
                       x = 0.125, y = 0.925)
    } else {
        vp <- viewport(just = c("right", "top"), width = 0.25, height = 0.25, 
                       x = 0.975, y = 0.925)
    }
    
    # create plot
    print(mainPlot)
    print(subPlot, vp = vp)


    ## check if to save the figure
    if (!missing(fig.name)) {
        file.name = paste(fig.name, ".pdf", sep = "")
        dev.copy(pdf, file.name, width = 8.5, height = 8.5) 
        dev.off()        
    }    
}