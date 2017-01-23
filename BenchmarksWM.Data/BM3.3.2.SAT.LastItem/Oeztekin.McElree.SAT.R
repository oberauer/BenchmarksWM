# Oeztekin & McElree (2010) SAT of Sternberg task

library(tidyr)
library(plyr)
library(dplyr)
library("Hmisc")
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

bgk <- c("black", "grey20", "grey40", "grey60", "grey80", "white")

for (id in 1:19) {
  datafile <- paste(getwd(), "/Oeztekin.McElree.2010/S", as.character(id), ".dat", sep="")
  dat <- read.table(datafile, header=F)
  dat <- cbind(rep(id, dim(dat)[1]), dat)
  if (id == 1) SATdat <- dat
  if (id > 1) SATdat <- rbind(SATdat, dat)
}
SATdat <- SATdat[,c(1:4, 15:19)]
names(SATdat) <- c("id", "trial", "cond", "lag", "flag", "lagrt", "rt", "corr", "confidence")

SATdat <- SATdat[SATdat$flag==0 & SATdat$rt < 600,]  # eliminate not-to-be-used trials (according to Ilke Oeztekin)

SATdat$serpos[is.element(SATdat$cond, c(0,1))] <- 1
SATdat$serpos[is.element(SATdat$cond, c(2,3))] <- 2
SATdat$serpos[is.element(SATdat$cond, c(4,5))] <- 3
SATdat$serpos[is.element(SATdat$cond, c(6,7))] <- 4
SATdat$serpos[is.element(SATdat$cond, c(8,9))] <- 5
SATdat$serpos[is.element(SATdat$cond, c(10,11))] <- 6
SATdat$ptype[is.element(SATdat$cond, 1:11)] <- 1  # all positive probes
SATdat$ptype[is.element(SATdat$cond, c(12:14))] <- 3  # all lures from 1st category
SATdat$ptype[is.element(SATdat$cond, c(15:17))] <- 4  # all lures from 2nd category
SATdat$ptype[is.element(SATdat$cond, c(18:20))] <- 5  # recent negative lure from previous trial
SATdat$ptype[is.element(SATdat$cond, c(21:23))] <- 2  # new lure

SATpos <- subset(SATdat, ptype==1)

x11(8,6)
maxx <- 3.5
miny <- 0.4
plot(0,0, xlim=c(0, maxx),ylim=c(miny,1), type="n", las=1,
     xlab="Lag + RT (s)", ylab="Proportion Correct",cex.lab=1,xaxt="n",yaxt="n", cex.axis=1)
for (sp in 1:6) {
  sat <- subset(SATpos, serpos==sp)
  satwide <- aggregate(corr ~ id+lag, data=sat, FUN=mean) %>% spread( lag, corr)
  satagg <- Confint(Bakeman(satwide[,2:8]))
  timewide <- aggregate(lagrt ~ id+lag, data=sat, FUN=mean) %>% spread(lag, lagrt)
  time <- colMeans(timewide[,2:8])/1000
  par(new=T)
  errbar(time, y=satagg[1,], yplus=satagg[2,], yminus=satagg[3,], type="b", pch=21, lty=1,
         bg=bgk[sp], errbar.col="black", xlim=c(0,maxx), ylim=c(miny,1), xlab="", ylab="")
}
legend(maxx,miny,c("SP=1", "SP=2", "SP=3", "SP=4", "SP=5", "SP=6"), pch=21, pt.bg = bgk, xjust=1, yjust=0)

