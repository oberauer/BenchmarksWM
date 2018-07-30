#Read Data of Barrouillet et al. (2011)

#remove all data and functions
rm(list=ls())

library(readxl)

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)
source(paste(dirname(getwd()), "/functions/lineplot.ci4.R", sep=""))

# Read Data ---------------------------------------------------------------------------------------------------

# Parity judgment experiment
Parity4 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="B17:D33")
names(Parity4) <- c("subject", "ptime", "span")
Parity4$task <- "Parity-4"
Parity6 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="E17:F33")
Parity6 <- cbind(Parity4[,"subject"], Parity6)
names(Parity6) <- c("subject", "ptime", "span")
Parity6$task <- "Parity-6"
Parity8 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="G17:H33")
Parity8 <- cbind(Parity4[,"subject"], Parity8)
names(Parity8) <- c("subject", "ptime", "span")
Parity8$task <- "Parity-8"
Parity <- rbind(Parity4, Parity6, Parity8)
Parity$totaltime <- 6900
Parity$CL <- Parity$ptime/Parity$totaltime  # cognitive load = processing time / total time

# Size judgment experiment
Size4 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="B17:D33")
names(Size4) <- c("subject", "ptime", "span")
Size4$task <- "Size-4"
Size6 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="E17:F33")
Size6 <- cbind(Size4[,"subject"], Size6)
names(Size6) <- c("subject", "ptime", "span")
Size6$task <- "Size-6"
Size8 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="G17:H33")
Size8 <- cbind(Size4[,"subject"], Size8)
names(Size8) <- c("subject", "ptime", "span")
Size8$task <- "Size-8"
Size <- rbind(Size4, Size6, Size8)
Size$totaltime <- 6900
Size$CL <- Size$ptime/Size$totaltime  

# Color Stroop and number Stroop experiments - for these experiments, processing time is constant
# across participants because it was assessed in a separate pre-test

ColorStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="B13:F33"))
names(ColorStroop) <- c("subject", "ptimeNeutral", "ptimeColor", "spanNeutral", "spanColor")
CStroopNeutral <- ColorStroop[,c("subject", "ptimeNeutral", "spanNeutral")]
names(CStroopNeutral) <- c("subject", "ptime", "span")
CStroopNeutral$task <- "ColorStroopNeutral"
CStroopColor <- ColorStroop[,c("subject", "ptimeColor", "spanColor")]
names(CStroopColor) <- c("subject", "ptime", "span")
CStroopColor$task <- "ColorStroopColor"
ColorStroop <- rbind(CStroopNeutral, CStroopColor)
ColorStroop$totaltime <- 8500
ColorStroop$CL <- ColorStroop$ptime/ColorStroop$totaltime  

NumberStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="I13:M33"))
names(NumberStroop) <- c("subject", "ptimeNeutral", "ptimeNumber", "spanNeutral", "spanNumber")
NStroopNeutral <- NumberStroop[,c("subject", "ptimeNeutral", "spanNeutral")]
names(NStroopNeutral) <- c("subject", "ptime", "span")
NStroopNeutral$task <- "NumberStroopNeutral"
NStroopNumber <- NumberStroop[,c("subject", "ptimeNumber", "spanNumber")]
names(NStroopNumber) <- c("subject", "ptime", "span")
NStroopNumber$task <- "NumberStroopNumber"
NumberStroop <- rbind(NStroopNeutral, NStroopNumber)
NumberStroop$totaltime <- 8500
NumberStroop$CL <- NumberStroop$ptime/NumberStroop$totaltime  

# Updating experiments
# Span procedure, according to Barrouillet et al: 
# "Two units were added to spans to take into account the fact that the processing task
# required the continuous maintenance of two items"

RunningCount <- read_excel("Barrouillet.2011.xlsx",sheet="Updating",range="B14:F33")
names(RunningCount) <- c("subject", "ptimeUpdate", "ptimeSimple", "spanUpdate", "spanSimple")
RCountSimple <- RunningCount[, c("subject", "ptimeSimple", "spanSimple")]
names(RCountSimple) <- c("subject", "ptime", "span")
RCountSimple$task <- "Simple Storage"
RCountUpdate <- RunningCount[, c("subject", "ptimeUpdate", "spanUpdate")]
names(RCountUpdate) <- c("subject", "ptime", "span")
RCountUpdate$task <- "Running Count"
RunningCount <- rbind(RCountSimple, RCountUpdate)
RunningCount$totaltime <- 14000
RunningCount$CL <- RunningCount$ptime/RunningCount$totaltime  

Nback <- read_excel("Barrouillet.2011.xlsx",sheet="Updating",range="J14:N38")
names(Nback) <- c("subject", "ptime0back", "ptime2back", "span2back", "span0back")
NB0 <- Nback[, c("subject", "ptime0back", "span0back")]
names(NB0) <- c("subject", "ptime", "span")
NB0$task <- "0-back"
NB2 <- Nback[, c("subject", "ptime2back", "span2back")]
names(NB2) <- c("subject", "ptime", "span")
NB2$task <- "2-back"
Nback <- rbind(NB0, NB2)
Nback$totaltime <- 12500
Nback$CL <- Nback$ptime/Nback$totaltime  

Data <- as.data.frame(rbind(Parity, Size, ColorStroop, NumberStroop, RunningCount, Nback))
Data$task <- as.factor(Data$task)

CL <- aggregate(CL ~ task, data=Data, FUN=mean)
Span <- aggregate(span ~ task, data=Data, FUN=mean)

pt=c(24, 24, 23, 23, 23, 23, 21, 21, 21, 24, 24, 22, 22, 22) # markers corresponding to Barrouillet et al (2011)
col=rep("black", 14)
ptcol=c("red", "red", "blue", "blue", "blue", "blue", "black", "black", "black", "red", "red", "green", "green", "green")
Tasks <- unique(Data$task)
xlim <- c(0,1)
ylim <- c(0,9)


x11()
plot(CL$CL, Span$span, xlim=xlim, ylim=ylim, type="p", pch=pt, col=col, bg=ptcol, 
     xlab="CL", ylab="Span")        
errorCircles(x="CL", y="span", data=Data, group="task", add=T,
             xlim = xlim, ylim = ylim, labels="",
             arrow.len = 0.05, alpha = 0.05, sd = FALSE, bars = TRUE, circles = F, 
             pch=pt, xlab="", ylab="") 
abline(a=8.13, b=-8.33)
legend(1,9,c("Updating", "Inhibition", "Response Selection", "Retrieval"), 
             pch=c(24,23,22,21), pt.bg=c("red", "blue", "green", "black"), yjust=1, xjust=1)
