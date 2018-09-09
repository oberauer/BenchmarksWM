########### Figure 7: Serial-Position Effects on Accuracy ##############

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
library("Hmisc")
source(paste(dirname(getwd()), "/functions/plot.confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci3.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
ptypes <- c(21:25, 21:25)

## forward and backward serial recall (Madigan, 1971)

madigan <- read.table("Madigan.1971.txt", colClasses=c("character"), header=F)
M <- as.data.frame(matrix(0, dim(madigan)[1], 11))
for (line in 1:dim(madigan)[1]) {
  M[line,1] <- as.numeric(substring(madigan[line,], 1,2))
  for (col in 2:11) {
    M[line,col] <- as.numeric(substring(madigan[line,], first=col+1, last=col+1))
  }
}
names(M) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8")
# 1 = audio, 2 = visual; 1 = forward, 2 = backward

accuracy <- t(apply(M[,4:11], MARGIN=1, FUN=function(x){as.numeric(as.numeric(x)==c(1:8))}))
Macc <- cbind(M[,1:3], accuracy)
names(Macc) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8") 
Magg <- aggregate(cbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8) ~ id+modality+direction, data=Macc, FUN=mean)
names(Magg) <- c("id", "modality", "direction", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8") 

## global and local recognition (Oberauer, 2003)

globrec <- read.table("Oberauer.2003.GlobRec.dat", header=T)
locrec <- read.table("Oberauer.2003.LocRec.dat", header=TRUE, dec=".")

global.PCpos.inpos.random <- Bakeman(globrec[,2:7])
global.PCpos.inpos.forward <- Bakeman(globrec[,14:19])
local.PCpos.inpos.random <- Bakeman(locrec[,2:7])
local.PCpos.inpos.forward <- Bakeman(locrec[,14:19])

x11(10,7)
layout(matrix(1:2,1,2))
for (mod in 2:2) {
  for (dir in 1:2) {
    d <- subset(Magg, modality==mod & direction == dir)
    dd <- Bakeman(d[,4:11])
    plot.confint(dd, 1:8, 1:8, ((dir-1)*2+mod)*0.1-0.25, xlim=c(0.5,8.5), ylim=c(0,1), cex=1.3, 
                 xlab="Serial Position", ylab="Proportion correct", lty=3-mod, pch=20+dir, bg=colors[dir])
    par(new=T)
  }
}
legend(0.55, 1, legend=c("Forward", "Backward"), pt.bg=colors[1:2], 
       lty=c(1,1), pch=ptypes[c(1,2)], pt.cex = 1.5, xjust=0, yjust=1)
title("Serial Recall")


ylimpc <- c(0.5,1)
legendtext <- c("Global", "Local")
legendtext <- c("Item Recognition", "Relational Recognition")
plot.confint(global.PCpos.inpos.random, c(1:6), c(1:6), -0.05, xlim = c(0.5,6.5), ylim=ylimpc, pch=ptypes[1], cex=1.3,
             lty=1, bg=colors[1], xlab="Serial Position", ylab="Proportion correct")
par(new=T)
plot.confint(local.PCpos.inpos.random, c(1:6), c(1:6), 0.05, xlim = c(0.5,6.5), ylim=ylimpc, pch=ptypes[2], cex=1.3,
             lty=1, bg=colors[2], xlab="Serial Position", ylab="Proportion correct")
par(new=T)
legend(0.55, ylimpc[2], legend=legendtext, 
       pt.bg=colors[1:2], lty=c(1,1,2,2), pch=ptypes[c(1,2)], pt.cex = 1.5, xjust=0, yjust=1)
title("Recognition")


### Continuous reproduction of colors in forward serial order (Peteranderl & Oberauer, 2017)

source(paste(dirname(getwd()), "/functions/wrap.R", sep=""))

colordata <- read.table("Peteranderl.2017.long.dat", header=F)
names(colordata)= c("id", "session", "trial", "serpos", 
                    "target", "response", "iti1", "iti2", "iti3", "iti4", "time")

# session codes AS condition: 1 = no AS, 2 = AS

colordata$targetrad <- pi*colordata$target/180
colordata$responserad <- pi*colordata$response/180
colordata$diffrad <- wrap(colordata$responserad - colordata$targetrad)
colordata$errorrad <- abs(colordata$diffrad)
colordata$errordeg <- 180*colordata$errorrad/pi
aggdat <- aggregate(errordeg ~ id + session + serpos, data = colordata, FUN=mean)

x11()
lineplot.ci3(data=aggdat, dv="errordeg", iv=c("serpos", "session"), id=1, x=1, off=0.05, ylim=c(0,80),
             cex=1.3, lty=1, pt = ptypes[c(1,2)], ptcol=colors[1:2], xlab="Serial Position", ylab="Error (deg)")
legend(4,0,c("Silent", "AS"), pt.bg=colors[1:2], lty=c(1,1), pch=ptypes[c(1,2)], pt.cex = 1.5, yjust=0)

