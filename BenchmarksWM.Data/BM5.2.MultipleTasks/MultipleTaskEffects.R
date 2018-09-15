# Within-Domain and Cross-domain effects of processing on storage (Chein, Moore, & Conway, 2011)

rm(list=ls())
graphics.off()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)

#obtain data
chein <- read.table("Chein.XSPANdata.txt",header=TRUE,sep=",")
chein$TEST <- ordered(chein$TEST,levels=c("LEX.LET","SYM.LET","LET", "SYM.LOC","LEX.LOC", "LOC"))

#plot barchart
p2f <- 0
if (p2f) {
  pdf(file="Chein11F2.pdf",height=8,width=10)
} else {x11(height=8,width=10)}

legendtext <- c("Lexical", "Symmetry", "None", "Symmetry", "Lexical", "None")
barspacing <- c(0.2,0.2,0.2,0.7,0.2,0.2)
bp = bargraph.CI(x.factor=TEST, response=SCORES, space=barspacing,
                 col=c(rep("dark gray",3),rep("light gray",3)),
                 cex.names=1.3, cex.lab = 1.3, las=1, ylim=c(1.9,4.4), cex.axis=1.3,
                 ylab="Number of Items Recalled", xlab="Type of Test", axisnames=F,
                 ci.fun=function(x) {c(mean(x) - se(x), mean(x) + se(x))}, 
                 data=chein)
text (2,4.2,"Letter Memory",cex=1.5)
text (6,4.2,"Location Memory",cex=1.5)
box()
axis(1, bp$xvals, labels=legendtext, cex.axis=1.3)
if (p2f) {dev.off()}


