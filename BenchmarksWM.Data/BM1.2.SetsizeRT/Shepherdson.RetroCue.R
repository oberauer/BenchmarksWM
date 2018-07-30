########## Set-Size and Retro-Cue effects on RTs in Change Detection (Shepherdson, Oberauer, & Souza, 2017) ########

rm(list=ls())
graphics.off()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))


ptypes <- c(21:25, 21:25)
colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")


## Read raw data of Experiment S1b (= Exp. 1b in Souza et al. (2014))
filename = "./Shepherdson.RetroCue/Souza1b(L200H5000).csv"
d1 <- read.csv(filename, header=T)  


## Read raw data of Experiment S1a (visual data = Exp. 1a in Souza et al. (2014))
filename = "./Shepherdson.RetroCue/VisualVerbal(L200H5000).csv"
d2 <- read.csv(filename, header=T)  

## Read raw data of Experiment 2
filename = "./Shepherdson.RetroCue/word_data(L200H5000).csv"
d3 <- read.csv(filename, header=T)  



## Produce a plot of hits and correct rejections by set size for part of the data of Eexperiment S1b

nsubj <- length(unique(d1$subj_idx))
Setsize <- sort(unique(d1$size))
Ptype <- unique(d1$probe)
NoCue <- array(NA,dim=c(4,3,nsubj))
for (ss in 1:4) {
  for (p in 1:3) {
    d <- subset(d1, size==Setsize[ss] & probe==Ptype[p] & CSI=="No")
    aggdat <- aggregate(correct ~ subj_idx, data=d, FUN=mean)
    NoCue[ss,p,] <- aggdat$correct
  }
}
Cue20 <- array(NA,dim=c(4,3,nsubj))
for (ss in 1:4) {
  for (p in 1:3) {
    d <- subset(d1, size==Setsize[ss] & probe==Ptype[p] & CSI=="2000")
    aggdat <- aggregate(correct ~ subj_idx, data=d, FUN=mean)
    Cue20[ss,p,] <- aggdat$correct
  }
}

x11()
layout(matrix(1:2, 1, 2))
lineplot.ci(Setsize, data=NoCue, off=0, xlim=c(1,6), ylim=c(0.5,1), 
            pt=ptypes, ptcol=colors[1:3], xlab="Set Size", ylab="P(correct)", main="No Cue")
legend(1, 0.5, c("Pos", "New", "Intrus"), pch=ptypes[1:3], pt.bg=colors[1:3], yjust=0)
lineplot.ci(Setsize, data=Cue20, off=0, xlim=c(1,6), ylim=c(0.5,1), 
            pt=ptypes, ptcol=colors[1:3], xlab="Set Size", ylab="P(correct)", main="Cue Delay 2.0 s")
legend(1, 0.5, c("Pos", "New", "Intrus"), pch=ptypes[1:3], pt.bg=colors[1:3], yjust=0)


