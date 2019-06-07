## function to plot the TIC of one run

plotTIC <- function(Run = list(), file.name = character()) {
    
    # to get rid of the 'no visible binding for global variable' notes.
    tic <- NULL
    
    # if a single run is provided, then the TIC of the run is plotted
    if (!missing(Run)) {        
        runTIC <- ggplot(data = data.frame(rt = Run$rt/60, tic = Run$tic), 
                        aes(x = rt, y = tic)) + 
                        geom_line(color = "blue", size = 0.5) + 
                        xlab("Time (min)") + ylab("TIC") + theme_bw() +
                        theme(axis.title.x = element_text(size = rel(1.5))) +
                        theme(axis.title.y = element_text(size = rel(1.5))) +
                        theme(plot.title = element_text( size=25)) +
                        theme(axis.text.x = element_text(size=10)) +
                        theme(axis.text.y = element_text(size=10))   
        
        print(runTIC)
    } 
    # if a set of file name(s) are provided, no graph is plotted, instead, all
    # TICs are saved as png files with the same name as file.name provides
    else if (missing(file.name)){
        stop("Either Run or file.name should be provided!")
    } else {
        num.runs <- length(file.name)
        
        Run <- readRDS(file = paste(file.name[i], '.rds', sep = ""))
                
        for (i in (1:num.runs)) {
            runTIC <- ggplot(data = data.frame(rt = Run$rt/60, tic = Run$tic), 
                            aes(x = rt, y = tic)) + 
                            geom_line(color = "blue", size = 0.5) +
                            ggtitle(file.name[i]) +
                            xlab("Time (min)") + ylab("TIC") + theme_bw() +
                            theme(axis.title.x = element_text(size = rel(1.5)))+
                            theme(axis.title.y = element_text(size = rel(1.5)))+
                            theme(plot.title = element_text( size=25)) +
                            theme(plot.title = element_text(vjust=1)) +
                            theme(axis.text.x = element_text(size=1)) +
                            theme(axis.text.y = element_text(size=1))   
            
            fig.name = paste(file.name[i], '.pdf', sep = "")
            ggsave(filename = fig.name, plot = runTIC, width = 8.5, height = 11, 
                   units = "in", dpi = 300)
        }
    }
}