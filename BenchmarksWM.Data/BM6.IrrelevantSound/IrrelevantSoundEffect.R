# Irrelevant-Sound effect and changing-state effect on serial-order memory (Schlittmacher et al.)

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)

library(readxl)
library(Hmisc)
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

irrspeech <- read_excel("Vp-Daten SJS-Diss Sprache.xls",sheet=4,col_names=T)  # sheet 4: visual presentation, IS throughout
irrmusic <- read_excel("Vp-Daten SJS-Diss Musik.xls",sheet=3,col_names=T)     # sheet 3: visual presentation, IS throughout

# subtract irrspeech from 100 because data are error percentages!
changing <- (100-irrspeech[,which(names(irrspeech)=="item1ch.st."):which(names(irrspeech)=="item9ch.st.")])/100
steady <- (100-irrspeech[,which(names(irrspeech)=="item1st.st."):which(names(irrspeech)=="item9st.st.")])/100
quiet <- (100-irrspeech[,which(names(irrspeech)=="item1ruhe"):which(names(irrspeech)=="item9ruhe")])/100
IS <- Confint(Bakeman(cbind(quiet, steady, changing)))

stakkato <- (100-irrmusic[,which(names(irrmusic)=="pos1stakkato"):which(names(irrmusic)=="pos9stakkato")])/100
legato <- (100-irrmusic[,which(names(irrmusic)=="pos1legato"):which(names(irrmusic)=="pos9legato")])/100
mquiet <- (100-irrmusic[,which(names(irrmusic)=="pos1ruhe"):which(names(irrmusic)=="pos9ruhe")])/100
IM <- Confint(Bakeman(cbind(mquiet, legato, stakkato)))

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")
x11(10,7)
layout(matrix(1:2,1,2))
for (cond in 1:3) {
  dd <- IS[, ((cond-1)*9+1):(cond*9)]
  errbar(1:9, y=dd[1,], yplus=dd[2,], yminus=dd[3,], xlab="Serial Position", ylab="Proportion Correct",
         xlim=c(0.5,9.5), ylim=c(0,1), pch=20+cond, bg=colors[cond], type="b", cex=1.3)
  if (cond < 3) par(new=T)
}
legend(1,0,c("Quiet", "Steady", "Changing"), lty=1, pch=21:23, pt.bg=colors[1:3], pt.cex=1.3, yjust=0)
title("Irrelevant Speech")
for (cond in 1:3) {
  dd <- IM[, ((cond-1)*9+1):(cond*9)]
  errbar(1:9, y=dd[1,], yplus=dd[2,], yminus=dd[3,], xlab="Serial Position", ylab="Proportion Correct",
         xlim=c(0.5,9.5), ylim=c(0,1), pch=20+cond, bg=colors[cond], type="b", cex=1.3)
  if (cond < 3) par(new=T)
}
legend(1,0,c("Quiet", "Legato", "Staccato"), lty=1, pch=21:23, pt.bg=colors[1:3], pt.cex=1.3, yjust=0)
title("Irrelevant Music")


