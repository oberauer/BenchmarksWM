##################### Figure 10: SPC of Recall Latencies in Serial Recall #######

library(sciplot)
library(tidyr)
library(plyr)
library(dplyr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))

fl04 <- read.table("Farrell.Lewandowsky.2004.Exp2.dat",header=FALSE)  # Data from Farrell  & Lewandowsky, 2004, Exp. 2
names(fl04) <- c("subject", "trial", "condition",paste("opp",c(1:6),sep=""),paste("rt",c(1:6),sep=""))
#The columns are subject, trial, condition  (0=no  interference, 1 = interference), 
#the recall order (columns are output positions), and the recall latencies
#intrusions prevented, omissions coded as -9

fl04rt   <- fl04 %>% gather("spos","rt",rt1:rt6) %>% 
  select(subject,trial,condition,spos,rt) %>%
  mutate(spos=as.numeric(substr(spos,3,10)))

fl04rtagg <- aggregate(rt ~ subject + condition + spos, data=fl04rt, FUN=mean)
fl04RT <- BakemanL(fl04rtagg, id="subject", dv="rt")

fl04m     <- aggregate(rt~condition+spos, data=fl04RT, FUN=mean)
fl04SE    <- aggregate(rt~condition+spos, data=fl04RT, FUN=function(x) sd(x)/sqrt(length(x))) #SE for within-subjects comparisons

#now do some plotting
p2f <- 0
if (p2f) {
  pdf(file="Farrell04spc.pdf",height=12,width=8)
} else {x11(height=12,width=8)}
#par(mfrow=c(2,1))  #accuracy and then RT
bgk <- c("gray","black")
ltyk <- c("solid","dashed")

#now plot RT by serial position
plot(0,0, xlim=c(0.5,6.5),ylim=c(0,3100), type="n", las=1,
     xlab="Serial Position", ylab="Mean Latency (ms)",cex.lab=1.2,cex.axis=1.)
for (k in c(0:1)) {
  xx <- tbp$spos - 0.05 + k*0.1
  tbp <- filter(fl04m,condition==k)
  lines(xx,tbp$rt,lwd=2,lty=ltyk[k+1])
  tbpSE <- filter(fl04SE,condition==k)
  arrows(xx,tbp$rt-1.96*tbpSE$rt, xx, tbp$rt+1.96*tbpSE$rt, length=0.05, angle=90, code=3)    
  points(xx,tbp$rt,pch=21+k,bg=bgk[k+1],cex=1.5)
}
legend(6,2900,c("No interference","Interference"),lty=ltyk,pch=20+c(1:2),pt.bg=bgk,cex=1.,pt.cex=1.3, xjust=1)
if (p2f) {dev.off()}


########################## Read Murdock & Okada (1970) data: Inter-Response Times for Free Recall 

MO70 <- read.table('MurdockOkada.1970.txt', header=F)
names(MO70) <- c("RunningCount", "ID", "trial", 
                 "inpos1", "inpos2", "inpos3","inpos4", "inpos5", "inpos6","inpos7", "inpos8", "inpos9","inpos10", "inpos11", "inpos12","inpos13",
                 "time1", "time2", "time3","time4", "time5", "time6","time7", "time8", "time9","time10", "time11", "time12","time13")
# inpos is the input serial position recalled, ordered by output position (according to Kahana, code 88 signals an extralist intrusion, but that is probably 99) 


