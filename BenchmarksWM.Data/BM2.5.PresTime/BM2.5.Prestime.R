############ Figure 6: Effect of presentation time: Tan & Ward (2008) and Bays et al (2011) ########

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

library("Hmisc")
library("circular")
source(paste(dirname(getwd()), "/functions/plot.confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")

# Tan & Ward (2008)
TW <- read.table("Tan.Ward.2008.dat", header=F)  # produced by Matlab/analyses/TanWard2008Analysis.m
names(TW) <- c("id", "trial", "ptime", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6")
Ptimes <- sort(unique(TW$ptime))
TWagg <- aggregate(cbind(sp1,sp2,sp3,sp4,sp5,sp6) ~ id+ptime, data=TW, FUN=mean)

# Bays et al
VWMdat <- read.table("Bays.2011.dat", header=F)
names(VWMdat) <- c("id", "setsize", "pt", "itemsep", "angle", "response", "error")
VWMdat$dev <- abs(VWMdat$error)
VWMdat$pt <- VWMdat$pt/1000  # convert to sec
VWMdat$errorrad <- pi*VWMdat$error/180
ptvector <- sort(unique(VWMdat$pt)) #presentation times
ssvector <- sort(unique(VWMdat$setsize))
idvector <- sort(unique(VWMdat$id))
darray <- array(NA, dim=c(length(ptvector), length(ssvector), length(idvector)))
for (tt in 1:length(ptvector)) {
  for (ss in 1:length(ssvector)) {
    d <- subset(VWMdat, setsize==ssvector[ss] & pt==ptvector[tt])
    aggd <- aggregate(errorrad ~ id, data=d, FUN=sd.circular)
    darray[tt,ss,] <- 1/aggd$errorrad
  }
}


x11(10,7)
layout(matrix(1:2,1,2))
lineplot.ci(ptvector, data=darray, off=0.01, xlim=c(0,1.05), ylim=c(0,6), ptcol=colors, 
            pt=ptypes, lty=1:4, xlab="Presentation Time (s)", ylab="Mean Precision (1/rad)")
legend(1,0,c("1", "2", "4", "6"), pch=ptypes, lty=1:4, pt.bg=colors, xjust=1, yjust=0)
title("Reproduction of Colors")

for (pt in 1:3) {
  dd <- subset(TWagg, ptime==Ptimes[pt])
  spc <- Bakeman(dd[,3:8])
  plot.confint(spc, 1:6, 1:6, (pt-2)*0.1, xlim=c(0.5,6.5), ylim=c(0,1), 
               xlab="Serial Position", ylab="P(correct)", lty=pt, pch=20+pt, col="black", bg=colors[pt])
  if (pt < 3) par(new=T)
}
legend(0.5,0,c("1 s / item", "2.5 s / item", "5 s / item"), pch=21:23, pt.bg=colors[1:3], lty=1:3, yjust=0)
title("Serial Recall of Words")
