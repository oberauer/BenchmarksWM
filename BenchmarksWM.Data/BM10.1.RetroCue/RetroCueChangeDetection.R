# Retro-cue effect in change detection (Souza et al., 2014)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

Retrocue1 <- read.table("SouzaRetroCueVisVerb.dat", header=F)
names(Retrocue1) <- c("id", "session", "trial", "visverb", "cuecond", "setsize", "color1", "color2", "color3", "color4", "color5", "color6", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "cuedloc", "ptype", "probecolor", "rt", "corr")
# visverb: 1 = visual, 2 = verbal
# cuecond: 1 = RC(100 ms), 2 = RC(400ms), 3 = RC(2000ms), 4 = no cue
Retrocue2 <- read.table("SouzaRetroCueVis.dat", header=F)
names(Retrocue2) <- c("id", "session", "trial", "cuecond", "setsize", "color1", "color2", "color3", "color4", "color5", "color6", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "cuedloc", "ptype", "probecolor", "rt", "corr")
# cuecond: 1 = RC(100 ms), 2 = RC(400ms), 3 = RC(1000ms), 4 = RC(2000 ms) 5 = no cue
# loc = location of stimulus (angle)
# ptype: 1 = match, 2 = intrusion, 3 = new probe


idvec <- unique(Retrocue2$id)
ssvec <- sort(unique(Retrocue2$setsize))
nsubj <- length(idvec)
darray <- array(0, dim=c(length(ssvec),5,nsubj))
condvec <- c(5, 1:4)  #put no-cue first
for (cond in 1:5) {
  for (ss in 1:length(ssvec)) {
    d <- subset(Retrocue2, cuecond==condvec[cond] & setsize==ssvec[ss])
    aggd <- aggregate(corr ~ id, data=d, FUN=mean) 
    darray[ss, cond, ] <- aggd$corr
  }
}

#colors <- c("red", "blue", "darkgreen", "magenta", "black", "green", "purple", "brown")
ptypes <- c(21:25, 21:25)
bgcolors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
x11()
lineplot.ci(ssvec, data=darray, off=0.05, xlim=c(1,6.3), ylim=c(0.75,1), ptcol=bgcolors, cex=1.2,
            lty=c(1,2,2,1,2), pt=ptypes, xlab="Setsize", ylab="P(correct)")
legend(1,0.75,c("No Cue", "100 ms", "400 ms", "1000 ms", "2000 ms"), pt.bg=bgcolors, pt.cex=1.2,
       lty=c(1,2,3,1,2), pch=ptypes, yjust=0)

