#Grenfell-Essam & Ward first-recall probabilities

library(readxl)
library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

colors <- c("black", "grey50", "grey80", "white")

plotpanel <- function(post,title4panel) {
  plot(0,0, xlim=c(0,max(post$`List Length`)+1),ylim=c(0,1), type="n", las=1, xaxt="n",
       xlab="List Length", ylab="Probability of First Recall",cex.lab=1.2)
  k<-0
  for (rx in unique(post$strategy)) {
    tbp<-filter(post, strategy==rx)
    k<-k+1
    lines(tbp$`List Length`, tbp$`Probability of First Recall`,lwd=2,lty=k)  
    points(tbp$`List Length`, tbp$`Probability of First Recall`,pch=20+k, bg=colors[k],cex=1.5)
  }
  text(7.5,.95,title4panel,cex=1.2)
  legend(12,.6,unique(post$strategy),lty=c(1:k),pch=c(21:(20+k)),pt.bg=colors,pt.cex=1.3,y.intersp = 1)
  par(tcl= -0.2)  #minor ticks
  axis(1, at=seq(from=0,to=max(post$`List Length`)+1,by=1), labels=F, lwd=1, lwd.ticks=0.5)
  par(tcl= -0.5)  #major ticks with labels
  axis(1, at=seq(from=0,to=max(post$`List Length`)+1,by=3), labels=seq(from=0,to=max(post$`List Length`)+1,by=3),
       lwd=0, lwd.ticks=1)
}

#read only the sheet as a separate data frame
gew <- read_excel("GrenfellEssamWard4figure.xlsx",sheet=1, col_names=F)
names(gew) <- c("cueing","strategy",paste("LL",c(2,	4,	5,	6,	7,	8,	12,	15),sep=""))

#gather and extract and so on
gew2<-  gather(gew,"List Length","Probability of First Recall",starts_with("LL"))
gew2[[3]] <- as.numeric(substr(gew2[[3]],3,5))
post <- filter(gew2, substr(cueing,1,4)=="Post")
pre <- filter(gew2, substr(cueing,1,3)=="Pre")

#now do some plotting
p2f <- 0
if (p2f) {
  pdf(file="grenfward.pdf",height=8,width=14)
} else {x11(height=6,width=10)}

par(mfrow=c(1,2),mar=c(4, 4, 2, 1) + 0.2)
plotpanel(pre,"Pre-cued")
plotpanel(post,"Post-cued")

if (p2f) {dev.off()}
