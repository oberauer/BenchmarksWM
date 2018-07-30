# Effect of cognitive load of processing demand on memory in complex-span task (Barrouillet et al., 2007)


library(readxl)
library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

#remap levels into consistent nomenclature after gathering
remap <- function (span, vnames) {
  s<-span$'Cognitive Load'
  s[span$'Cognitive Load'==vnames[1]] <- "low"
  s[span$'Cognitive Load'==vnames[2]] <- "medium"
  s[span$'Cognitive Load'==vnames[3]] <- "high"
  return(s)
}

#total time available for distractor
totaltime <- 6900

#extract the rows that contain data
temp <- read_excel("Barrouillet.E3.xls",sheet=1,col_names=FALSE) [c(3:18,33:48),]
#convert into numbers
b07 <- as.data.frame(lapply(temp,FUN=function(x) as.numeric(as.character(x))))
#add identifier for task
b07$task <- c(rep("parity",16),rep("location",16))

#now convert into suitable format
span<-gather(b07,"Cognitive Load","Mean Span",X5,X9,X13) [,c("X1","task","Cognitive Load","Mean Span")]
span$'Cognitive Load' <- remap(span,c("X5","X9","X13"))

tontask<-gather(b07,"Cognitive Load","Time on task",X2,X6,X10) [,c("X1","task","Cognitive Load","Time on task")]
tontask$'Cognitive Load' <- remap(tontask,c("X2","X6","X10"))

pcnr <-gather(b07,"Cognitive Load","Pct NR",X3,X7,X11) [,c("X1","task","Cognitive Load","Pct NR")]
pcnr$'Cognitive Load' <- remap(pcnr,c("X3","X7","X11"))

serfait <- gather(b07,"Cognitive Load","Ser Fait",X4,X8,X12) [,c("X1","task","Cognitive Load","Ser Fait")]
serfait$'Cognitive Load' <- remap(serfait,c("X4","X8","X12"))

#merge all dependent measures together with identifiers into final data set
b07fin<-Reduce(function(...) merge(...),list(span,tontask,pcnr,serfait))
names(b07fin)[1] <- "participant"
b07fin <- BakemanL(b07fin, id="participant", dv="Mean Span")

#average across participants for display
b07means <- aggregate(cbind(`Time on task`,`Mean Span`)~ task + `Cognitive Load`, data=b07fin, FUN=mean)
b07SE <- aggregate(cbind(`Time on task`,`Mean Span`)~ task + `Cognitive Load`, data=b07fin, FUN=function(x) {sd(x)/sqrt(length(x))})
b07means$'Time on task' <- b07means$'Time on task'/totaltime

#now do some plotting
p2f <- 0
if (p2f) {
  pdf(file="barrouillet07.pdf",height=6,width=6)
} else {x11(height=6,width=6)}

plot(0,0, xlim=c(0.2,.6),ylim=c(0,7), type="n", las=1,
     xlab="Total Processing Time/Total Time", ylab="Mean Span",cex.lab=1)
abline(lm(`Mean Span` ~ `Time on task`, data=filter(b07means, task=="parity"))$coefficients,lwd=2.5)
abline(lm(`Mean Span` ~ `Time on task`, data=filter(b07means, task=="location"))$coefficients,lwd=2.5,lty="dashed",col="gray")
xx <- filter(b07means, task=="location")$'Time on task'
mn <- filter(b07means, task=="location")$'Mean Span'
se <- filter(b07SE, task=="location")$'Mean Span'
points(xx,mn,cex=1.3,pch=21,bg="gray")
arrows(xx,mn-1.96*se, xx, mn+1.96*se, length=0.05, angle=90, code=3)   
xx <- filter(b07means, task=="parity")$'Time on task'
mn <- filter(b07means, task=="parity")$'Mean Span'
se <- filter(b07SE, task=="parity")$'Mean Span'
points(xx,mn,cex=1.3,pch=22,bg="black")
arrows(xx,mn-1.96*se, xx, mn+1.96*se, length=0.05, angle=90, code=3) 
legend(.3,2,c("Parity","Location"),lty=c("solid","dashed"),pch=c(22,21),pt.bg=c("gray","black"), pt.cex=1.2)

if (p2f) {dev.off()}
