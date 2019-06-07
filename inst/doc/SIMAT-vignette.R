### R code from vignette source 'SIMAT-vignette.Rnw'

###################################################
### code chunk number 1: packageLoad
###################################################
# load the package
library(SIMAT)
# load the extracted data from a CDF file of a SIM run
data(Run)

# load the target table information
data(target.table)

# load the background library to be used with fragment selection
data(Library)

# load retention index table from RI standards
data(RItable)


###################################################
### code chunk number 2: examineRun
###################################################
# check the names of different fields in Run
names(Run)

# show some values for the the first three fields
head(as.data.frame(Run[c("rt", "sc", "tic")]))

# see what is included in the scan information for the first scan
Run$pk[[1]]


###################################################
### code chunk number 3: plotTIC
###################################################
# plot the TIC of the selected Run
plotTIC(Run = Run)


###################################################
### code chunk number 4: examineTargetTable
###################################################
# check the name of included fields
names(target.table)

# check the first lines of the target.table
head(as.data.frame(target.table[c("compound", "numFrag")]))

# check the contents of the ms field
target.table$ms[[1]]


###################################################
### code chunk number 5: examineLibrary
###################################################
# check the name of included fields
names(Library)

# check the first lines of Library
head(as.data.frame(Library[c("rt", "ri", "compound")]))

# check the contents of the ms and sp fields related to the mass and intensity
# of the fragments, i.e. spectral information
Spectrum <- data.frame(ms = Library$ms[[1]], sp = Library$sp[[1]])
head(Spectrum)

# plot the spectrum
plot(x = Spectrum$ms, y = Spectrum$sp, type = "h", lwd = 2, col = "blue", 
     xlab = "mass", ylab = "intensity", main = Library$compound[1])


###################################################
### code chunk number 6: examineRItable
###################################################
# check the name of included fields
names(RItable)

# check the first lines of RItable
head(RItable)


###################################################
### code chunk number 7: getTarget
###################################################
# get targets info using target table and provided library
Targets <- getTarget(Method = "library", Library = Library, 
                    target.table = target.table)

# check the fields of Targets
names(Targets)

# check the first lines of some fields
head(as.data.frame(Targets[c("compound", "rt", "ri")]))


###################################################
### code chunk number 8: getPeak
###################################################
# get the peaks for this run corresponding to Targets
runPeaks <- getPeak(Run = Run, Targets = Targets)

# check the length of runPeaks (number of targets)
length(runPeaks)

# check the fields for each peak
names(runPeaks[[1]])

# area of the EIC of the first target
runPeaks[[1]]$area


###################################################
### code chunk number 9: plotEIC
###################################################
# plot the EIC of the first peak (target) on the list
plotEIC(peakEIC = runPeaks[[1]])


###################################################
### code chunk number 10: getRI
###################################################
# create the RI calibration function
calcRI <- getRI(RItable)

# calculate the RI of an RT = 12.32min
calcRI(12.32)

# get the peaks for this run corresponding to Targets using RI calibration
runPeaksRI <- getPeak(Run = Run, Targets = Targets, calcRI = calcRI)


###################################################
### code chunk number 11: getPeakScore
###################################################
# find the similarity score of the found targets
Scores <- getPeakScore(runPeaks = runPeaks, plot = TRUE)

# check the value of scores
print(Scores)


###################################################
### code chunk number 12: optFrag
###################################################
# get the optimized version of the target list
optTargets <- optFrag(Library = Library, target.table = target.table, 
                      forceOpt = TRUE)

# check the fragments of the first target
# the mass of fragments
Targets$ms[[1]]
# the intensity of fragments
Targets$sp[[1]]

# check them after optimization
# the mass of fragments
optTargets$ms[[1]]
# the intensity of fragments
optTargets$sp[[1]]


