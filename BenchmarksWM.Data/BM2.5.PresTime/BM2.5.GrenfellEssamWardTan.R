####### Read Data from Grenfell-Essam, Ward, & Tan (2013), Experiment 1 #######

rm(list=ls())
graphics.off()

library("readxl")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

ptypes <- c(21:25, 21:25)
colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")

# Load data for free-recall experiment
data = as.data.frame(read_excel("Grenfell-EssamWard&Tan(2013)JEPLM&C.xls"))  
# include: 1=yes, 2=no; Suppression: "AS" = articulatory suppression, "NO AS" = no articulatory suppression

# add numerical presentation time (in s)
data$prestime <- 0
data[data$blockrate=="f",]$prestime <- 0.5
data[data$blockrate=="m",]$prestime <- 1
data[data$blockrate=="s",]$prestime <- 3

LL <- sort(unique(data$length))
PP <- sort(unique(data$prestime))

dataAS <- subset(data, Suppression=="AS")
dataSilent <- subset(data, Suppression=="No AS")
SS.Silent <- unique(dataSilent$subj)
SS.AS <- unique(dataAS$subj)

aggdatSilent <- array(NA, dim=c(length(LL), 3, length(SS.AS)))
aggdatAS <- array(NA, dim=c(length(LL), 3, length(SS.AS)))
for (L in 1:length(LL)) {
  for (P in 1:3) {
    for (S in 1:length(SS.AS)) {
      subdatSilent <- subset(dataSilent, length==LL[L] & prestime==PP[P] & subj==SS.Silent[S])
      aggdatSilent[L,P,S] <- mean(subdatSilent$FRcorrect)
      subdatAS <- subset(dataAS, length==LL[L] & prestime==PP[P] & subj==SS.AS[S])
      aggdatAS[L,P,S] <- mean(subdatAS$FRcorrect)    
      }
  }
}

x11()
layout(matrix(1:2, 1, 2))
lineplot.ci(LL, data=aggdatSilent, off=0, xlim=c(1,max(LL)), ylim=c(0,1), 
            pt=ptypes, ptcol=colors[1:3], xlab="List Length", ylab="P(correct)", main="Silent")
legend(1, 0, c("0.5s", "1.0s", "3.0s"), pch=ptypes[1:3], pt.bg=colors[1:3], yjust=0)
lineplot.ci(LL, data=aggdatAS, off=0, xlim=c(1,max(LL)), ylim=c(0,1), 
            pt=ptypes, ptcol=colors[1:3], xlab="List Length", ylab="P(correct)", main= "Articulatory Suppression")
legend(1, 0, c("0.5s", "1.0s", "3.0s"), pch=ptypes[1:3], pt.bg=colors[1:3], yjust=0)


