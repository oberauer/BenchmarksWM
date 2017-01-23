# Effect of set size and size of change on change-detection (Keshvari et al, 2013)

source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
CD <- read.table("WeijiChangeDetection.dat", header=F)
names(CD) <- c("id", "setsize", "delta", "response")

# plot CD
nbins <- 11
deltabins <- 0.0001+seq(from=0, to=0.5*pi, length.out=nbins)
idvector <- unique(CD$id)
ssvector <- unique(CD$setsize)

propResp <- array(dim=c(length(ssvector), nbins, length(idvector)))
for (subj in idvector) {
  for (ss in 1:length(ssvector)) {
    d <- subset(CD, id==subj & setsize==ssvector[ss])
    dd <- subset(d, delta==0)
    propResp[ss,1,subj] <- mean(dd$response)
    for (ch in 2:nbins) {
      dd <- subset(d, delta>deltabins[ch-1] & delta<deltabins[ch])
      propResp[ss,ch,subj] <- mean(dd$response)
    }
  }
}

colors <- c("black", "gray50", "gray80", "white")
x11()
lineplot.ci(deltabins, propResp, xdim=2, xlab="Size of Change (rad)", ylab="P('change')", 
            pt=21:24, ptcol=colors, cex=1.2)
legend(max(deltabins),0, legend=c("N=1", "N=2", "N=4", "N=8"), pch=c(21:24), pt.cex=1.2,
       pt.bg=colors, lty=1, xjust=1,yjust=0)

