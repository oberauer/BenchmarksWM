# Plot Delayed-Estimation effect of set size on response distributions
# Data from Van den Berg et al. (2012, PNAS)


#remove all data and functions
rm(list=ls())

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)

source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci3.R", sep=""))
source(paste(dirname(getwd()), "/functions/wrap.R", sep=""))


# Read color DE data, simple version (used for plot in the Benchmarks paper)
DE <- read.table("WeijiDelayedEstimation.dat", header=F)
names(DE) <- c("id", "setsize", "error")
DE$error_rad <- pi*DE$error/180


# plot DE
nbins <- 21
breakpoints <- seq(-pi, pi, length.out=nbins+1)
idvector <- unique(DE$id)
ssvector <- unique(DE$setsize)
errorFreq <- array(dim=c(length(ssvector), nbins, length(idvector)))
for (subj in idvector) {
  for (ss in 1:length(ssvector)) {
    d <- subset(DE, id==subj & setsize==ssvector[ss])
    h <- hist(d$error_rad, breaks=breakpoints, plot=F)
    errorFreq[ss,,subj] <- h$counts
  }
}

x11()
lineplot.ci(h$mids, errorFreq, type="l", lty=1:4, xdim=2, xlab="Error (rad)", ylab="Mean Frequency")
legend(pi,max(errorFreq), legend=c("N=1", "N=2", "N=4", "N=8"), lty=1:4, xjust=1,yjust=1)


## Read color DE data and orientation DE data from Van den Berg et al (2012), richer version

Color <- read.table("vandenberg_et_al_2012_color_long.dat", header=F)
Orient <- read.table("vandenberg_et_al_2012_orient_long.dat", header=F)
names(Color) <- c("id", "trial", "exp", "setsize", 
                  "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                  "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8",
                  "response")
names(Orient) <- c("id", "trial", "exp", "setsize", 
                  "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                  "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8",
                  "response")
# stim1 to stim8 are the angles (1 to 360) of the stimuli (in case of colors: angles in the color wheel). The experiments distinguished only 180 angles, so the angles were doubled to scale them into a range from 1 to 360 (degrees) in steps of 2.
# loc1 to loc8 refer to the 8 possible locations of stimuli in the array. 
# stim1 is the target stimulus, and loc1 is the target location
# stimuli that did not exist (set sizes < 8) are set to 360; locations that did not exist are set to 0. 
# In Color, exp=3 is the experiment using the color wheel, and exp=4 is the experiment using scrolling. In Orient, this variable is meaningless. 

Color$targetrad <- pi*Color$stim1/180
Color$responserad <- pi*Color$response/180
Color$devrad <- wrap(Color$responserad - Color$targetrad)
Color$errorrad <- abs(Color$devrad)

Orient$targetrad <- pi*Orient$stim1/180
Orient$responserad <- pi*Orient$response/180
Orient$devrad <- wrap(Orient$responserad - Orient$targetrad)
Orient$errorrad <- abs(Orient$devrad)

Coloragg <- aggregate(errorrad ~ id + exp + setsize, data=Color, FUN=mean)
Orientagg <- aggregate(errorrad ~ id + setsize, data=Orient, FUN=mean)
x11()
layout(matrix(1:2,1,2))
lineplot.ci3(Coloragg, iv=c("setsize", "exp"), dv="errorrad", xlim=c(0.5, 8.5), ylim=c(0, 2), 
             xlab="Set Size", ylab="Error (rad)", main="Color")
legend(1,2,c("Color Wheel", "Scrolling"), pch=c(15,16))
lineplot.ci3(Orientagg, iv="setsize", dv="errorrad", xlim=c(0.5, 8.5), ylim=c(0, 2), 
             xlab="Set Size", ylab="Error (rad)", main="Color")

