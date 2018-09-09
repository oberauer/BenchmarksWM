# Retro-cue effect in continuous reproduction (Oberauer & Lin, 2017)

rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/wrap.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci3.R", sep=""))


colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
ptypes <- c(21:25, 21:25)


Retrocue <- read.table("colorwheel5.dat", header=F)
names(Retrocue) <- c("id", "session", "cue", "xxx", "trial", "setsize", "cuecond", "cuelocation", 
                      "stim1", "loc1", "stim2", "loc2", "stim3", "loc3", "stim4", "loc4", "stim5", "loc5", "stim6", "loc6", "stim7", "loc7", "stim8", "loc8", "response")

# cue: 1 = informative cue, 2 = neutral cue
# cuecond: -1 = neutral, 1 = valid, 0 = invalid
# stim1 to stim8: colors on the color wheel (in degrees), stim1 is the target
# loc1 to loc8: locations of the colors on the virtual circle (1 to 13), loc1 is the target location

Retrocue$targetrad <- pi*Retrocue$stim1/180
Retrocue$responserad <- pi*Retrocue$response/180
Retrocue$diffrad <- wrap(Retrocue$responserad - Retrocue$targetrad)
Retrocue$errorrad <- abs(Retrocue$diffrad)
Retrocue$errordeg <- 180*Retrocue$errorrad/pi

aggdat <- aggregate(errordeg ~ id + setsize + cuecond, data=Retrocue, FUN=mean)
x11()
lineplot.ci3(data=aggdat, dv="errordeg", iv=c("setsize", "cuecond"), id=1, x=1, off=0.05, ylim=c(0,80),
             cex=1.3, lty=1, pt = ptypes[1:3], ptcol=colors[1:3], xlab="Set Size", ylab="Error (deg)")
legend(4,0,c("Neutral", "Invalid", "Valid"), pt.bg=colors[1:3], lty=1, pch=ptypes[1:3], pt.cex = 1.5, yjust=0)

