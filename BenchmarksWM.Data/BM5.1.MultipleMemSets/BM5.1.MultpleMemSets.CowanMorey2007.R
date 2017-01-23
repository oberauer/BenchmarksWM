###################### Figure 14: Cross-Domain Set-Size Effect (Cowan & Morey 2007) ###########

library(plyr)
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")

# Load data
d=read.csv("Cowan.Morey.2007.csv", header = T, stringsAsFactors = F, strip.white = T)
aggd <- aggregate(response.ACC ~ probeDomain + Domain + Subject, data=d, FUN="mean")
aggd <- BakemanL(aggd, id="Subject", dv="response.ACC")
nsubj <- length(unique(aggd$Subject))
darray <- array(0, dim=c(2,3,nsubj))
domainvec <- c("single", "cross", "within")
pdomainvec <- c("Verb", "Vis")
for (dom in 1:3) {
  for (pdom in 1:2) {
    dd <- subset(aggd, Domain==domainvec[dom] & probeDomain==pdomainvec[pdom])
    darray[pdom, dom, ] <- dd$response.ACC
  }
}

x11(4,6)
lineplot.ci(1:2, data=darray, off=0.02, xlim=c(0.7,2.3), ylim=c(0.5,1), ptcol=colors, cex=1.3,
            lty=c(1,2,2), pt=ptypes, xlab="Tested Domain", ylab="P(correct)", xaxt="n")
axis(1, at=c(1,2), labels=pdomainvec)
legend(0.75,0.5,c("Single", "Cross", "Within"), pt.bg=colors, pt.cex=1.3,
       lty=c(1,2,2), pch=ptypes, yjust=0)
