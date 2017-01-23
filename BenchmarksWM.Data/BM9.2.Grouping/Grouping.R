# Effects of grouping on accuracy and serial-position curves (Frankish, 1989)


library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
library(plotrix)
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

#function to rearrange data into column format
reformatdata <- function(aud) {
  aud2 <- aud %>% gather("subject","listrec",s01:s16) %>% 
    #  select(subject,list,listrec,cond,trial) %>% 
    mutate(listrec=as.character(listrec),list=as.character(list)) %>%
    separate(list,paste("li",c(1:9),sep=""),sep=c(1:8),convert=TRUE) %>%
    separate(listrec,paste("r",c(1:9),sep=""),sep=c(1:8),convert=TRUE) 
  
  audlist <- aud2 %>% gather("spos","item",li1:li9) %>% select(-(r1:r9)) %>% 
    mutate(spos=as.numeric(substr(spos,3,10))) %>% arrange(subject,trial,spos)
  audresp <- aud2 %>% gather("spos","resp",r1:r9) %>% select(-(li1:li9)) %>% 
    mutate(spos=as.numeric(substr(spos,2,10))) %>% arrange(subject,trial,spos)
  return(inner_join(audlist,audresp,by=c("subject","trial","cond","spos")))
}

#function to plot each modality in a separate graph
ploteachmodality <- function(audsubmeans,fname,titletext) {
  bgk <- c("gray","black","gray","black","white")
  
  audmeans <- aggregate(pc ~ cond + spos, data=audsubmeans, FUN=mean)
  audSE    <- aggregate(pc ~ cond + spos, data=audsubmeans, FUN=function(x) sd(x)/sqrt(length(x)))
  
  plot(0,0, xlim=c(0.5,9.5),ylim=c(0.2,1), type="n", las=1, xaxt="n", yaxt="n",
       xlab="Serial Position", ylab="Proportion Correct",cex.lab=1.4,cex.axis=1.2)
  for (k in c(1:5)) {  #run over cond
    tbp <- filter(audmeans,cond==k)
    lines(tbp$spos[1:3],tbp$pc[1:3],lwd=2,lty=k)
    lines(tbp$spos[4:6],tbp$pc[4:6],lwd=2,lty=k)
    lines(tbp$spos[7:9],tbp$pc[7:9],lwd=2,lty=k)
    
    tbpSE <- filter(audSE,cond==k)
    arrows(tbp$spos,tbp$pc-1.96*tbpSE$pc, tbp$spos,tbp$pc+1.96*tbpSE$pc, length=0.05, angle=90, code=3)    
    points(tbp$spos,tbp$pc,pch=20+k,bg=bgk[k],cex=1.3)
  }
  axis(1, at=c(1:9),lwd=1, lwd.ticks=1,cex.axis=1.2)
  axis.break(2,.25,style="slash") 
  axis(2, at=seq(from=.3,to=1.,by=.1),lwd=1, lwd.ticks=1,cex.axis=1.2)
  legend(1,.5,c("Ungrouped","0.25 s", "0.5 s", "1 s", "2 s"),lty=c(1:5),pch=20+c(1:5),pt.bg=bgk,cex=1.2,pt.cex=1.2)
  title(titletext)
}

#read data
aud <- read_excel("Frankish.1989.Exp1234.xlsx",sheet="Exp 1 auditory", col_names=TRUE,skip=2)
aud <- aud[1:60,1:19]
vis <- read_excel("Frankish.1989.Exp1234.xlsx",sheet="Exp 1 visual",col_names=TRUE,skip=2)
vis <- vis[1:60,1:19]
#reshape the data into the required column format
audfin <- reformatdata(aud)
visfin <- reformatdata(vis)

#compute proportion correct for plotting, first at subject level
audsubmeans <- audfin %>% group_by(cond, spos, subject) %>% summarise(pc = sum(resp==item)/12)
vissubmeans <- visfin %>% group_by(cond, spos, subject) %>% summarise(pc = sum(resp==item)/12)
audsubmeans <- BakemanL(as.data.frame(audsubmeans), id="subject", dv="pc")
vissubmeans <- BakemanL(as.data.frame(vissubmeans), id="subject", dv="pc")
#ridiculously, although adusbumeans is allegedly a data frame, it is in fact a tibble that has to be converted into a data frame!


#now obtain group means for plotting and do plot
p2f <- 0

if (p2f) {
  pdf(file=fname,height=8,width=14)
} else {x11(height=8,width=14)}
par(mfrow=c(1,2))
ploteachmodality(audsubmeans, "Frankish89aud.pdf", "Auditory")
ploteachmodality(vissubmeans, "Frankish89vis.pdf", "Visual")
if (p2f) {dev.off()}


