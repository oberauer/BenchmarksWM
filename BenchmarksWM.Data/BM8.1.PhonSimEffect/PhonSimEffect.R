# Phonological similarity effects in adults and children


library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

# Load and prepare Farrell & Lewandowsky, 2003, Experiment 1 & 3

plotselection <- 2
# 1 = Plot just all-similar and all-dissimilar lists for Exp. 1, 2 = plot all conditions

dat1 <- read.table("Farrell.Lsky.normed1201.txt",
                  col.names = c("subject",
                                "trial",
                                "scond",
                                "c1","c2","c3","c4","c5","c6",
                                "o.1","o.2","o.3","o.4","o.5","o.6",
                                "rt1","rt2","rt3","rt4","rt5","rt6"))

# subject = subject number; trial = trial number, scond = similarity condition: 0 = all D, 1 = all S, 2 = D in Pos 2, 3 = D in Pos 4, 4 = D in Pos 6, 5 = SDSDSD
# c1 to c6: similar (0) or dissimilar (1) item in input positions 1 to 6
# o.1 to o.6: input position of the item reported in output positions 1 to 6; values > 6 are extralist intrusions; 0 = omissions. 
# rt1 to rt6: response times for outputs 1 to 6. 

dat2 <- read.table("psim2dat.ed",
                   col.names = c("subject",
                                 "trial",
                                 "scond",
                                 "c1","c2","c3","c4","c5","c6",
                                 "o.1","o.2","o.3","o.4","o.5","o.6",
                                 "rt1","rt2","rt3","rt4","rt5","rt6"))

tdat1 <- dat1 %>% gather(serposo, op, o.1:o.6)
tdat1 <- tdat1 %>% separate(serposo, into = c("oo", "serpos"), sep = "\\.") 
tdat1$acc <- tdat1$serpos==tdat1$op

tdat2 <- dat2 %>% gather(serposo, op, o.1:o.6)
tdat2 <- tdat2 %>% separate(serposo, into = c("oo", "serpos"), sep = "\\.") 
tdat2$acc <- tdat2$serpos==tdat2$op

if (plotselection == 1) tdat1 <- tdat1[(tdat1$scond==0) | (tdat1$scond==1),]

smeans1 <- ddply(tdat1, .(serpos,scond,subject), summarise, meanacc=mean(acc))  #subject means
#ameans <- ddply(smeans, .(scond,serpos), summarise, mean=mean(meanacc))       #aggregated means
nsubj <- length(unique(smeans1$subject))
darray1 <- array(0,dim=c(6,length(unique(tdat1$scond)),nsubj))
for (sp in 1:max(smeans1$serpos)) {
  for (sim in sort(unique(tdat1$scond))) {
    d <- subset(smeans1, serpos==sp & scond==sim)
    darray1[sp,sim+1,] <- d$meanacc
  }
}

smeans2 <- ddply(tdat2, .(serpos,scond,subject), summarise, meanacc=mean(acc))  #subject means
#ameans <- ddply(smeans, .(scond,serpos), summarise, mean=mean(meanacc))       #aggregated means
nsubj <- length(unique(smeans2$subject))
darray2 <- array(0,dim=c(6,length(unique(tdat2$scond)),nsubj))
Simconds <- sort(unique(tdat2$scond))
for (sp in 1:max(smeans2$serpos)) {
  for (sim in 1:length(Simconds)) {
    d <- subset(smeans2, serpos==sp & scond==Simconds[sim])
    darray2[sp,sim,] <- d$meanacc
  }
}


### Now Plot Farrell & Lewandowsky Data

ptypes <- c(21:25, 21:25)
colors <- c("black", "red", "blue", "blue", "blue", "green")
if (plotselection == 1) legendtext1 = c("Dissimilar", "Similar")
if (plotselection == 2) legendtext1 = c("Dissimilar", "Similar", "D in Pos 2", "D in Pos 4", "D in Pos 6", "SDSDSD")
legendtext2 = c("Dissimilar", "D in Pos 2", "D in Pos 4", "SDSDSD")

source("c:\\daten\\R\\toolbox\\lineplot.ci.R")
x11(height=12,width=8)
par(mfrow=c(2,1))

par(mar=c(4,4,2,2))
lineplot.ci(1:max(smeans1$serpos), data=darray1, off=0, xlim=c(1,6), ylim=c(0,1), col=colors, ptcol=colors,
            pt=ptypes, xlab="Serial Position", ylab="P(correct)")
legend(1,0, legendtext1, col=colors, pt.bg=colors, lty=1, pch=ptypes, yjust=0)
title("Experiment 1")

lineplot.ci(1:max(smeans2$serpos), data=darray2, off=0, xlim=c(1,6), ylim=c(0,1), col=colors[c(1,3,4,6)], ptcol=colors[c(1,3,4,6)],
            pt=ptypes, xlab="Serial Position", ylab="P(correct)")
legend(1,0, legendtext2, col=colors[c(1,3,4,6)], pt.bg=colors[c(1,3,4,6)], lty=1, pch=ptypes, yjust=0)
title("Experiment 3")





# Load Jarrold Data

j13 <- read.table("jarrold13.txt",header=TRUE, sep=",")
names(j13)[1] <- "Year"  #weird insertion of characters in variable name must be corrected

# Plot Jarrold Data

x11(height=12,width=8)
plot(0,0, xlim=c(0.8,4.2),ylim=c(-3,8), type="n", las=1,
     xlab="Grade Level", ylab="Phonological Similarity Effect",cex.lab=1,xaxt="n")
for (k in unique(j13$CONDITION)) {
  temp<-filter(j13, CONDITION==k)
  tbp <- aggregate((DISPC-SIMPC) ~ Year, temp, FUN=mean)
  lines(tbp$Year,tbp[[2]],lwd=2,lty=k)
  setbp <- aggregate((DISPC-SIMPC) ~ Year, temp, FUN=function(x) sd(x)/sqrt(length(x)))
  arrows(setbp$Year, tbp[[2]]-1.96*setbp[[2]], setbp$Year, tbp[[2]]+1.96*setbp[[2]], length=0.05, angle=90, code=3)    
  points(tbp$Year,tbp[[2]],pch=20+k,bg=colors[k],cex=1.3)
}
abline(h=0,lty="dotted")
axis(1, at=seq(from=1,to=4,by=1),labels=c("K","1","2","3"),lwd=0, lwd.ticks=1)
legend(4.2,-3,c("Visual-verbal","Visual-visual","Verbal-verbal","Verbal-visual"),lty=c(1:4), bg="white",
       pch=20+c(1:4),pt.bg=colors,pt.cex=1,xjust=1,yjust=0)
mtext("B", side=3, adj=0, line=1)


