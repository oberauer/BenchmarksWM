########## Figure 3: Set-Size and Serial-Position effects on RTs in Item Recognition (Chris Donkin) ########

rm(list=ls())
graphics.off()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))


ptypes <- c(21:25, 21:25)
colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")


## Read raw data
SternFast <- NULL
SternSlow <- NULL
for (subj in 1:7) {
  filename = paste("./Donkin.Sternberg/subj", as.character(subj), ".txt", sep="")
  d <- read.table(filename, header=T)  
  names(d)[1:2] <- c("x1", "id")
  d$id <- subj
  d <- d[,2:11]
  if (subj < 5) SternFast <- rbind(SternFast, d) #Chris: subj 1-4 did the fast version
  if (subj > 4) SternSlow <- rbind(SternSlow, d) #Chris: subj 5-7 did the slow version
}
SternFastAgg <- aggregate(RT ~ id + SetSize + Lag, data=SternFast[SternFast$use==1,], FUN=mean)
SternSlowAgg <- aggregate(RT ~ id + SetSize + Lag, data=SternSlow[SternSlow$use==1,], FUN=mean)

darrayF <- array(NA, dim=c(6, 5, 4))  #lag, setsize, subject
darrayS <- array(NA, dim=c(6, 5, 3))
for (ss in 1:5) {
  for (lg in 1:(ss+1)) {
      if (lg <= ss) lagidx <- lg  # "Lag" is actually serial position, counting from 1 to setsize
      if (lg > ss)  lagidx <- 6   # I suspect these are the new probes
      darrayF[lagidx,ss,] <- SternFastAgg$RT[SternFastAgg$SetSize==ss & SternFastAgg$Lag==lg] #order lag backwards!
      darrayS[lagidx,ss,] <- SternSlowAgg$RT[SternSlowAgg$SetSize==ss & SternSlowAgg$Lag==lg] #order lag backwards!
  }
}

SternbergTicks <- c("1", "2", "3", "4", "5", "New")
xlim <- c(1,6.5)
ylim <- c(0.35, 0.7)
x11()
layout(m=matrix(1:2,nrow=1,byrow=T))
par(mar=c(4,4,1,1))
lineplot.ci(1:5, data=darrayF[1:5,,], xdim=1, off=0.05, xlim=xlim, ylim=ylim, col="black", ptcol=colors,
            pt=ptypes, lty=1, xlab="Serial Position", ylab="RT(s)", xaxt="n")
newprobesF <- Bakeman(t(darrayF[6,,])) # transpose to have subjects in rows
npF <- Confint(newprobesF)
par(new=T) 
for (ss in 1:5) {
  errbar(6+0.05*(ss-1), npF[1,ss], yplus=npF[2,ss], yminus=npF[3,ss], xlim=xlim, ylim=ylim,add=T,
         type="b", pch=ptypes[ss], col=colors[ss], bg=colors[ss], errbar.col=colors[ss])
}
axis(1, at=1:6, SternbergTicks)
lineplot.ci(1:5, data=darrayS[1:5,,], xdim=1, off=0.05, xlim=xlim, ylim=ylim, col="black", ptcol=colors, 
            pt=ptypes, lty=1, xlab="Serial Position", ylab="RT(s)", xaxt="n")
newprobesS <- Bakeman(t(darrayS[6,,])) # transpose to have subjects in rows
npS <- Confint(newprobesS)
par(new=T) 
for (ss in 1:5) {
  errbar(6+0.05*(ss-1), npS[1,ss], yplus=npS[2,ss], yminus=npS[3,ss], xlim=xlim, ylim=ylim,add=T,
         type="b", pch=ptypes[ss], col=colors[ss], bg=colors[ss], errbar.col=colors[ss])
}
axis(1, at=1:6, SternbergTicks)
