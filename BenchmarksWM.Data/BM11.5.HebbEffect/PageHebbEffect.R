# Hebb effect (Page et al., 2006)


library("Hmisc")
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

bgk <- c("black","gray","black","black","white")

#read subject-level data
hebb<- read.table("Page.Hebb.txt", header=T)
names(hebb) <- c("subject","cond","order","supnosup",paste("t",c(1:16),sep=""))

hebbsubmeans <- hebb %>% gather("trial","pc",t1:t16) %>% mutate(trial=as.numeric(substr(trial,2,10))) %>%
  group_by(cond, supnosup, subject,trial) %>% summarise(pc = mean(pc))
hebbsubmeans <- BakemanL(as.data.frame(hebbsubmeans), id="subject", dv="pc")
hebbmeans <- aggregate(pc ~ cond + supnosup + trial, data=hebbsubmeans, FUN=mean)
hebbSE <- aggregate(pc ~ cond + supnosup + trial, data=hebbsubmeans, FUN=function(x) sd(x)/sqrt(length(x)))

#now do some plotting
p2f<-0
if (p2f) {
  pdf(file="pageHebb.pdf",height=12,width=10)
} else {x11(height=12,width=10)}
par(mfrow=c(2,1))
par(mar=c(4,4,2,2))
for (sns in c("ns","su")) {
  if (sns=="ns") {
    t4p <- "Silent"
  } else {
    t4p <- "Articulation"
  }
  plot(0,0, xlim=c(0.5,16.5),ylim=c(0.3,.8), type="n", las=1, xaxt="n", 
       xlab="Trial", ylab="Proportion Correct",cex.lab=1,cex.axis=1)
  
  allconds <- unique(hebbmeans$cond)
  for (k in c(2:length(allconds))) {  #run over cond (but not FillA)
    tbp <- filter(hebbmeans,cond==allconds[k] & supnosup==sns)
    se <- filter(hebbSE,cond==allconds[k] & supnosup==sns)
    tbpspos <- tbp$trial
    #lines(tbpspos,tbp$pc,lwd=2,lty=k-1)
    #points(tbpspos,tbp$pc,pch=20+k,bg=bgk[k],cex=1)
    par(new=T)
    errbar(tbpspos, y=tbp$pc, yplus=tbp$pc+1.96*se$pc, yminus=tbp$pc-1.96*se$pc, type="b", las=1,
           xlim=c(0.5,16.5),ylim=c(0.3,.8), xlab="", ylab="", pch=20+k, bg=bgk[k], errbar.col=bgk[k], cex=1)
  }
  axis(1, at=c(1:16),lwd=1, lwd.ticks=1,cex.axis=1)
  legend(4,.45,c("Filler","Repeating"),lty=c(1:2),pch=20+c(2:length(allconds)),pt.bg=bgk[2:3],cex=1,pt.cex=1)
  text(2.5,.75,t4p,cex=1)
}
if (p2f) {dev.off()}

