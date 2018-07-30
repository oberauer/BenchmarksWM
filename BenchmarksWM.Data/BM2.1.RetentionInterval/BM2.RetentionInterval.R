
############ Figure 5: Brown-Peterson (A), Forgetting with constant vs. variable distractors (B)

library(readxl)
library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
library(plotrix)
source(paste(dirname(getwd()), "/functions/plot.confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")

#read Floden data
temp <- read_excel("Floden.BrownPeterson.xlsx",sheet="Exp2_Data")
ncolumns <- dim(temp)[2]
temp <- temp[,1:(ncolumns-2)] #shave off last 2 columns, which are empty and headed "NA"
floden <- temp[,c("SUBJECT", "GROUP", "PROB_0", "PROB_3", "PROB_18", "PROB_36", "PROB_60")]

#compute means and CIs within each age group
flodenY <- subset(floden, GROUP==2)
flodenO <- subset(floden, GROUP==1)
flodenaggY <- Confint(Bakeman(flodenY[,3:7]))
flodenaggO <- Confint(Bakeman(flodenO[,3:7]))

#read Lewandowsky et al 2010 data
lsky10 <- read.table("Lewandowsky.2010.E2.dat",header=FALSE,stringsAsFactors = FALSE)
names(lsky10) <- c("subject", "dummy",  "cond", "trial", 
                   paste("li",c(1:5),sep=""),paste("distrt",c(1:5),sep=""),
                   paste("re",c(1:5),sep=""),paste("rt",c(1:5),sep=""))

lsky10resp <- lsky10 %>% mutate(r1=as.numeric(li1==re1), r2=as.numeric(li2==re2), 
                                r3=as.numeric(li3==re3), r4=as.numeric(li4==re4), r5=as.numeric(li5==re5)) %>%
              select(-(li1:li5),-(re1:re5),-(rt1:rt5),-(distrt1:distrt5),-dummy,-trial) 
lsky10resp$pc <- rowMeans(lsky10resp[,3:7])
lsky10wide <- aggregate(pc ~ subject+cond, data=lsky10resp, FUN=mean) %>% spread( cond, pc)
lsky10agg <- Confint(Bakeman(lsky10wide[,2:5]))

# read Ricker et al 2014
r14 <- read.table("Ricker.2014.txt",header=TRUE)
names(r14) <- c("subject","RI","ITI","accuracy","RT","corchar","respchar","setsize","ptrl")

#aggregate in steps to compute SE correctly
r14m1 <- aggregate(accuracy ~ subject+RI+ITI, data=r14, FUN=mean)
r14m1 <- BakemanL(r14m1, id="subject", dv="accuracy")
r14se <- aggregate(accuracy ~ RI+ITI, data=r14m1,FUN=function(x) 1.96*sd(x)/sqrt(length(x)))  
r14m <- aggregate(accuracy ~ RI+ITI, data=r14m1, FUN=mean)
r14m <- merge(r14m,rename(r14se,se=accuracy))
r14m$RI <- (r14m$RI+10)/1000  #add the missing 10 ms as per Ricker email and then convert to seconds


#now do some plotting
p2f <- 0
#par(mfrow=c(1,1))
if (p2f) {
  pdf(file="Floden00F3.pdf",height=8,width=12)
} else {x11(height=8,width=12)}
layout(matrix(1:2,1,2,byrow=T))
par(mar=c(4,4,4,2))

plot(0,0, xlim=c(-0.2,60.2),ylim=c(0,1), type="n", las=1,
     xlab="Retention Interval (Seconds)", ylab="Proportion Correct",cex.lab=1,xaxt="n",cex.axis=1)
bgk <- c("gray","black","gray","black","white")
ltyk <- c("solid","dashed")
RI <- c(0, 3, 18, 36, 60)
library("Hmisc")

#par(new=T)
errbar(RI, y=flodenaggY[1,], yplus=flodenaggY[2,], yminus=flodenaggY[3,], type="b", pch=21, lty=ltyk[1], axes=F,
       bg=bgk[1], errbar.col=bgk[1], xlim=c(-0.2,60.2), ylim=c(0,1), xlab="", ylab="", add=T)
#par(new=T)
errbar(RI, y=flodenaggO[1,], yplus=flodenaggO[2,], yminus=flodenaggO[3,], type="b", pch=22,  lty=ltyk[2], axes=F,
       bg=bgk[2], errbar.col=bgk[2], xlim=c(-0.2,60.2), ylim=c(0,1), xlab="", ylab="", add=T)
axis(1, at=RI,lwd=1, lwd.ticks=1,cex.axis=1)
legend(3,.35,c("Younger","Older"),lty=ltyk,pch=20+c(1:2),pt.bg=bgk,cex=1,pt.cex=1)
title("A: Forgetting in Brown-Peterson Paradigm")

plot(0,0, xlim=c(0.5, 4.5),ylim=c(0.3,1), type="n", las=1,
     xlab="Condition", ylab="Proportion Correct",cex.lab=1,xaxt="n",yaxt="n", cex.axis=1)
par(new=T)
errbar(1:4, y=lsky10agg[1,], yplus=lsky10agg[2,], yminus=lsky10agg[3,], type="b", pch=21, lty=ltyk[1], axes=F,
       bg=bgk[2], errbar.col=bgk[2], xlim=c(0.5,4.5), ylim=c(0.3,1), xlab="", ylab="")
axis(1, at=c(1:4),labels=c("Quiet","1 distractor","3 identical distr.","3 different distr."),lwd=1, lwd.ticks=1,cex.axis=1)
axis.break(2,.35,style="slash") 
axis(2, at=seq(from=.4,to=1.,by=.1),lwd=1, las=1, lwd.ticks=1,cex.axis=1)
title("B: Forgetting in Verbal Complex Span")


if (p2f) {dev.off()}
