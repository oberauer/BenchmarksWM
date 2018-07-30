########### Figure 4: Number of Items Remembered #######################

library("readxl")
library("Hmisc")
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

d = as.data.frame(read_excel("ChenCowan2009.xlsx"))
names(d)[1:2] <- c("group", "id") #group 2 = silent (from Chen & Cowan 2005) 4 = AS; subject numbers repeat across groups but are NOT the same people!
dAS <- subset(d, group==4)
chunkdata <- Confint(Bakeman(dAS[,3:12])) # each observation can be seen as a sample from a Poisson distribution, which can be approximated by a Normal
x11(10,6)
barX <- barplot(height=chunkdata[1,], xlim=c(0.5,12), ylim=c(0,4), ylab="Number of Chunks Recalled")
errbar(barX, y=chunkdata[1,], yplus=chunkdata[2,], yminus=chunkdata[3,], xlim=c(0.5,12), ylim=c(0,6), add=T, type="n")


