####### Read Grenfell-Essam & Ward (2012) data #######

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of this script as the current directory

rm(list=ls())
graphics.off()

library("Hmisc")
library("readxl")
library("stats")
source(paste(dirname(getwd()), "/functions/plot.confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))
source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

ptypes <- c(21:25, 21:25)
bgcolors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")


# Load data for Experiment 1 (Immediate Free Recall)
E1 = as.data.frame(read_excel("GEW.2012.JML.xlsx", sheet=1))  

# Load data for Experiment 2 (Immediate Serial Recall)
E2 = as.data.frame(read_excel("GEW.2012.JML.xlsx", sheet=2))  

# Load data for Experiment 3 (Immediate Free Recall and Serial Recall)
E3 = as.data.frame(read_excel("GEW.2012.JML.xlsx", sheet=3))  

E1$LengthKnow = 1
E1$LengthKnow[E1$Knownlength=="Unknown"] <- 0
E1num <- E1[,c("subj", "block", "list", "trialnumber","length", "serpos", "outputorder" )]

E2$TaskTypeKnow = 1
E2$TaskTypeKnow[E2$TaskType=="Unknown"] <- 0
E2num <- E2[,c("subj", "block", "list", "trialnumber", "TaskTypeKnow", "length", "serpos", "writtenSP", "outputorder")]

E3$prepost <- 1
E3$prepost[E3$PrePost=="POST"] <- 2
E3$cond <- 1
E3$cond[E3$condition=="SR"] <- 2
E3$cond[E3$condition=="POST"] <- 3
E3$group <- 1
E3$group[E3$Group=="PostFR"] <- 2
E3$group[E3$Group=="PreISR"] <- 3
E3$group[E3$Group=="PostISR"] <- 4
E3$tasktype <- 1
E3$tasktype[E3$task=="iSR"] <- 2
E3num <- E3[,c("subj", "block", "list", "trialnumber", "prepost", "condition", "group", "tasktype",
               "length", "serpos", "writtenSP", "outputorder")]


## Write into text file, with and without string variables
write.table(E1, file='GEW.2012.E1.txt', na="-1", row.names=F)
write.table(E1num, file='GEW.2012.E1.num.txt', na="-1", row.names=F, col.names=F)

write.table(E2, file='GEW.2012.E2.txt', na="-1", row.names=F)
write.table(E2num, file='GEW.2012.E2.num.txt', na="-1", row.names=F, col.names=F)

write.table(E3, file='GEW.2012.E3.txt', na="-1", row.names=F)
write.table(E3num, file='GEW.2012.E3.num.txt', na="-1", row.names=F, col.names=F)












wordspan <- d[,which(grepl("wor", colnames(d)))]
letterspan <- d[,which(grepl("let", colnames(d)))]
opspan <- d[,which(grepl("op", colnames(d)))]
rspan <- d[,which(grepl("rsp", colnames(d)))]
simplespan <- (wordspan + letterspan[,1:6])/2 #average the 2 simple spans for each subject and set size
complexspan <- (opspan + rspan)/2    #same for complex span
simple <- Confint(Bakeman(simplespan))
complex <- Confint(Bakeman(complexspan))

# Load data for running memory span
d = read_excel("Bunting.Cowan.Running.xls", sheet=3)  # data from Bunting & Cowan, Exp. 1
RSPCfast <- d[which(names(d)=="f7sp7_ac"):which(names(d)=="f1sp1_ac")]
RSPCslow <- d[which(names(d)=="s7sp7_ac"):which(names(d)=="s1sp1_ac")]
runningfast <- matrix(0,dim(RSPCfast)[1],7)
runningslow <- matrix(0,dim(RSPCslow)[1],7)
pointer <- 1
for (setsize in 7:2) {
  allsp <- pointer:(pointer+setsize-1)
  runningfast[,setsize] <- rowMeans(RSPCfast[,allsp])
  runningslow[,setsize] <- rowMeans(RSPCslow[,allsp])
  pointer <- pointer+setsize
}
runningfast[,1] <- as.matrix(RSPCfast[,pointer])
runningslow[,1] <- as.matrix(RSPCslow[,pointer])
runningfast <- Bakeman(runningfast)
runningslow <- Bakeman(runningslow)

#Load data for Memory Updating, Oberauer & Kliegl (2006)
colnames1 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit", 
              "corrval1", "resp1", "correct1", "rt1",
              "corrval2", "resp2", "correct2", "rt2",
              "corrval3", "resp3", "correct3", "rt3",
              "corrval4", "resp4", "correct4", "rt4")
mutaf1 <- read.table("Oberauer.Kliegl.MU1.dat", header=F, fill=T, col.names=colnames1) #with col.names given, read.table reads in as many columsn as there are names
colnames2 <- c("id", "setsize", "trial", "pt0", "pt1", "ptcat", "crit", 
               "corrval1", "resp1", "correct1", "rt1",
               "corrval2", "resp2", "correct2", "rt2",
               "corrval3", "resp3", "correct3", "rt3",
               "corrval4", "resp4", "correct4", "rt4",
               "corrval5", "resp5", "correct5", "rt5",
               "corrval6", "resp6", "correct6", "rt6")
mutaf2 <- read.table("Oberauer.Kliegl.MU2.dat", header=F, fill=T, col.names=colnames2) #with col.names given, read.table reads in as many columsn as there are names
mutaf1$exp = 1
mutaf2$exp = 2
mutaf1 <- mutaf1[mutaf1$setsize>0,]
pcidx1 <- which(grepl("correct", colnames(mutaf1)))
pcidx2 <- which(grepl("correct", colnames(mutaf2)))
ssidx <- which(colnames(mutaf1)=="setsize")
computePC <- function(x) {
  setsize <- as.numeric(x[1])
  pcvector <- as.numeric(x[2:(setsize+1)])
  return(mean(pcvector))}

mutaf1$PC <- NULL
for (j in 1:dim(mutaf1)[1]) {
  mutaf1[j,"PC"] <- computePC(mutaf1[j,c(ssidx, pcidx1)])
}
#mutaf1$pc <- apply(mutaf1[,c(ssidx, pcidx1)], MARGIN=2, FUN=computePC) # should do the same in theory, but does not work
mutaf2$PC <- NULL
for (j in 1:dim(mutaf2)[1]) {
  mutaf2[j,"PC"] <- computePC(mutaf2[j,c(ssidx, pcidx2)])
}
mt1 <- mutaf1[, which(colnames(mutaf1) %in% c("id", "exp", "setsize", "pt0", "PC"))]
mt2 <- mutaf2[, which(colnames(mutaf2) %in% c("id", "exp", "setsize", "pt0", "PC"))]
mutaf <- rbind(mt1, mt2)
mt1.y.long <- subset(mt1, id < 30 & pt0 > 5999)
mt2.y.long <- subset(mt2, id < 30 & pt0 > 5999)

nsubj <- length(unique(mt1.y.long$id))
MUarray1 <- array(NA,dim=c(4,1,nsubj))
for (ss in 1:4) {
    d <- subset(mt1.y.long, setsize==ss)
    aggdat <- aggregate(PC ~ id, data=d, FUN=mean)
    MUarray1[ss,1,] <- aggdat$PC
}
nsubj <- length(unique(mt2.y.long$id))
MUarray2 <- array(NA,dim=c(3,1,nsubj))
for (ss in 4:6) {
  d <- subset(mt2.y.long, setsize==ss)
  aggdat <- aggregate(PC ~ id, data=d, FUN=mean)
  MUarray2[ss-3,1,] <- aggdat$PC
}


#Item Recognition: McElree 1989 Exp 2
ss3 <- c(.09, .06, .05)
ss4 <- c(.13, .15, .08, .06)
ss5 <- c(.23, .26, .14, .08, .06)
ss6 <- c(.35, .30, .28, .13, .10, .05)
RecPE <- c(mean(ss3), mean(ss4), mean(ss5), mean(ss6))

#N-back: Jonides et al 1997
NbackPE <- c(0.03, 0.05, 0.065, 0.115)

#Nback: Verhaeghen & Basak 2005 young
NbackVerhaeghenY <- c(0.97, 0.96, 0.945, 0.92, 0.86)

# Change detection: Adam et al (2015)
CD <- read.table("Adam.ChangeDet.dat", header=F)
names(CD) <- c("id", "setsize", "change", "correct")
CDagg <- aggregate(correct ~ id+setsize, data=CD, FUN=mean)
library("tidyr")
CDwide <- CDagg %>% spread(setsize, correct)  # takes variable correct and writes into separate variables for each level of setsize


############# Start Plotting #####################

x11()
layout(matrix(1:6, 3, 2, byrow=T))

errbar(2:7, y=simple[1,], yplus=simple[2,], yminus=simple[3,], type="b", pch=ptypes[1], 
       bg=bgcolors[1], errbar.col=bgcolors[1], xlim=c(1,7), ylim=c(0,1), xlab="Set Size", ylab="P(correct)")
par(new=T)
errbar(2:5, y=complex[1,], yplus=complex[2,], yminus=complex[3,], type="b", pch=ptypes[2], 
       bg=bgcolors[2], errbar.col=bgcolors[2], xlim=c(1,7), ylim=c(0,1), xlab="Set Size", ylab="P(correct)")
legend(1, 0, c("Simple Span", "Complex Span"), pch=ptypes, pt.bg=bgcolors, yjust=0)
title("Serial Recall")
mtext("A", side=3, adj=0, line=1)



plot.confint(runningfast, 1:7, 1:7, type="b", pch=ptypes[1], bg=bgcolors[1], xlim=c(1,7), ylim=c(0,1), 
             xlab="Set Size", ylab="P(correct)")
par(new=T)
plot.confint(runningslow, 1:7, 1:7, type="b", pch=ptypes[2], bg=bgcolors[2], xlim=c(1,7), ylim=c(0,1), 
     xlab="Set Size", ylab="P(correct)")
title("Running Span")
legend(1, 0, c("Fast", "Slow"), pch=ptypes, pt.bg=bgcolors, yjust=0)
mtext("B", side=3, adj=0, line=1)      

plot(3:6, 1-RecPE, type="b", pch=ptypes[1], bg=bgcolors[1], xlim=c(1,6), ylim=c(0.5,1), 
     xlab="Set Size", ylab="P(hits)")
title("Item Recognition (Words)")
mtext("C", side=3, adj=0, line=1)

plot(0:3, 1-NbackPE, type="b", pch=ptypes[1], bg=bgcolors[1], xlim=c(0,5), ylim=c(0.5,1), 
     xlab="N", ylab="P(correct)")
par(new=T)
plot(1:5, NbackVerhaeghenY, type="b", pch=ptypes[2], bg=bgcolors[2], xlim=c(0,5), ylim=c(0.5,1), 
     xlab="N", ylab="P(correct)")
legend(0, 0.5, c("Standard", "Columns"), pch=ptypes, pt.bg=bgcolors, yjust=0)
title("N-back")
mtext("D", side=3, adj=0, line=1)


lineplot.ci(1:4, data=MUarray1, off=0, xlim=c(1,6), ylim=c(0.5,1), 
            pt=ptypes, ptcol=bgcolors[1], xlab="Set Size", ylab="P(correct)")
par(new=T)
lineplot.ci(4:6, data=MUarray2, off=0.05, xlim=c(1,6), ylim=c(0.5,1), 
            pt=ptypes, ptcol=bgcolors[2], xlab="Set Size", ylab="P(correct)")
legend(1,0.5,c("Study 1", "Study 2"), pch=ptypes, pt.bg=bgcolors, yjust=0)
title("Memory Updating")
mtext("E", side=3, adj=0, line=1)

plot.confint(CDwide, 2:6, 2:6, off=0, xlim=c(1,6.5), ylim=c(0.5,1), xlab="Set Size", ylab="P(correct)")
title("Change Detection")
mtext("F", side=3, adj=0, line=1)

