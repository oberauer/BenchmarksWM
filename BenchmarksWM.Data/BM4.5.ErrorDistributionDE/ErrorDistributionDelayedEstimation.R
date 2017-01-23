# Plot Delayed-Estimation effect of set size on response distributions


source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

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

source("c:\\daten\\R\\toolbox\\lineplot.ci.R")
x11()
lineplot.ci(h$mids, errorFreq, type="l", lty=1:4, xdim=2, xlab="Error (rad)", ylab="Mean Frequency")
legend(pi,max(errorFreq), legend=c("N=1", "N=2", "N=4", "N=8"), lty=1:4, xjust=1,yjust=1)


