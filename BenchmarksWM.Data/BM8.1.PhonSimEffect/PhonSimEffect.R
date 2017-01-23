# Phonological similarity effects in adults and children


library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

dat <- read.table("Farrell.Lsky.normed1201.txt",
                  col.names = c("subject",
                                "trial",
                                "scond",
                                "c1","c2","c3","c4","c5","c6",
                                "o.1","o.2","o.3","o.4","o.5","o.6",
                                "rt1","rt2","rt3","rt4","rt5","rt6"))

tdat <- dat %>% gather(serposo, op, o.1:o.6)
tdat <- tdat %>% separate(serposo, into = c("oo", "serpos"), sep = "\\.") 
tdat$acc <- tdat$serpos==tdat$op

tdat <- tdat[(tdat$scond==0) | (tdat$scond==1),]
smeans <- ddply(tdat, .(serpos,scond,subject), summarise, meanacc=mean(acc))  #subject means
#ameans <- ddply(smeans, .(scond,serpos), summarise, mean=mean(meanacc))       #aggregated means
nsubj <- length(unique(smeans$subject))
darray <- array(0,dim=c(6,2,nsubj))
for (sp in 1:max(smeans$serpos)) {
  for (sim in 0:1) {
    d <- subset(smeans, serpos==sp & scond==sim)
    darray[sp,sim+1,] <- d$meanacc
  }
}

j13 <- read.table("jarrold13.txt",header=TRUE, sep=",")
names(j13)[1] <- "Year"  #weird insertion of characters in variable name must be corrected


ptypes <- c(21:25, 21:25)
source("c:\\daten\\R\\toolbox\\lineplot.ci.R")
if (p2f) {
  pdf(file="jarrold13b.pdf",height=10,width=7)
} else {x11(height=12,width=8)}
par(mfrow=c(2,1))
par(mar=c(4,4,2,2))
lineplot.ci(1:max(smeans$serpos), data=darray, off=0, xlim=c(1,6), ylim=c(0,1), col=colors, ptcol=colors,
            pt=ptypes, pt.cex=1.5, xlab="Serial Position", ylab="P(correct)")
legend(1,0,c("Dissimilar", "Similar"), col=colors, pt.bg=colors, pt.cex=1.5, lty=1, pch=ptypes, yjust=0)
mtext("A", side=3, adj=0, line=1)

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
if (p2f) {dev.off()}

