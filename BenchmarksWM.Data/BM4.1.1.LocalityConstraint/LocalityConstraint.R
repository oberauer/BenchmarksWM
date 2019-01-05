# Locality constraint in serial recall and recall of visual arrays

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)

source(paste(dirname(getwd()), "/functions/Confint.R", sep=""))
source(paste(dirname(getwd()), "/functions/Bakeman.R", sep=""))

## Farrell & Lsky 2004, Exp. 2, Transposition Gradient
fl04 <- read.table("FL04_2.dat",header=FALSE)
names(fl04) <- c("subject", "trial", "condition",paste("opp",c(1:6),sep=""),paste("rt",c(1:6),sep=""))
#The columns are subject, trial, condition  (0=no  interference, 1 = interference), 
#the recall order (columns are output positions), and the recall latencies
#intrusions prevented, omissions coded as -9
Transgrad <- function(data) {
  tdist <- matrix(0,1,length(data))
  topp <- matrix(0,1, length(data))
  for (sp in 1:length(data)) {
    if (data[sp] > 0) {
      td <- abs(data[sp]-sp)
      tdopp <- abs(1:length(data)-sp)
      tdist[td] <- tdist[td] + 1
      topp[tdopp[tdopp>0]] <- topp[tdopp[tdopp>0]] + 1
    }
  }
  return(cbind(tdist, topp))
}

Outdat <- fl04[,which(names(fl04)=="opp1"):which(names(fl04)=="opp6")]
TD <- as.data.frame(t(apply(Outdat, MARGIN=1, FUN=Transgrad)))
names(TD) <- c("td1", "td2", "td3", "td4", "td5", "td6", "tdopp1", "tdopp2", "tdopp3", "tdopp4", "tdopp5", "tdopp6")
fl04td <- cbind(fl04, TD)
fl04tdagg    <- aggregate(cbind(td1, td2, td3, td4, td5, td6) ~ subject+condition, data=fl04td, FUN=mean)
fl04tdoppagg <- aggregate(cbind(tdopp1, tdopp2, tdopp3, tdopp4, tdopp5, tdopp6) ~ subject+condition, data=fl04td,  FUN=mean)
for (sp in 1:5) {
  tdname <- paste("td", as.character(sp), sep="")
  tdoppname <- paste("tdopp", as.character(sp), sep="")
  fl04tdagg[,tdname] <- fl04tdagg[,tdname]/fl04tdoppagg[,tdoppname]
}
tgrad0 <- Confint(Bakeman(fl04tdagg[fl04tdagg$cond==0,which(names(fl04tdagg)=="td1"):which(names(fl04tdagg)=="td5")]))
tgrad1 <- Confint(Bakeman(fl04tdagg[fl04tdagg$cond==1,which(names(fl04tdagg)=="td1"):which(names(fl04tdagg)=="td5")]))


## Rerko et al spatial gradient
spatgrad <- read.table("spatialGradientSerial.txt", header=F)
spatgrad <- spatgrad[,c(1:3,6:10,45:49,50)]
names(spatgrad) <- c("id", "session", "trial", "color1", "color2", "color3", "color4", "color5",
                     "dist1", "dist2", "dist3", "dist4", "dist5", "response")
selectedIdx <- apply(spatgrad[,c(4:8,14)], MARGIN=1, FUN=function(x){which(as.numeric(x[1:5])==as.numeric(x[6]))}) #which color is equal to the response
spatgrad$selectedIdx <- as.numeric(selectedIdx)
selectedDist <- apply(spatgrad[,c(9:13, 15)], MARGIN=1, FUN=function(x){x[x[6]]}) #which distance is the selected color
spatgrad$selectedDist <- as.numeric(selectedDist)
spatgradError <- subset(spatgrad, selectedDist > 0) #keep only distances > 0 (i.e., errors)

# baseline: draw from the possible errors at random
errorDist <- as.data.frame(t(apply(spatgrad[,c(9:13)], MARGIN=1, FUN=function(x){x[x>0]})))  #keep the 4 distances that are not 0
distSample <- NULL
nSamples <- 100
for (sample in 1:nSamples) {
  dsample <- apply(errorDist, MARGIN=1, FUN=function(x){x[sample(1:4,1)]})  # sample from the 4 distances at random -> pick error response at random
  distSample <- c(distSample, dsample)
}
nBins <- 6
histbounds <- as.numeric(quantile(distSample, probs=seq(0, 1, 1/nBins))) # histogram bounds selected so that random sampling results in equal frequencies
idvector <- unique(spatgrad$id)
densities <- matrix(NA, length(idvector), nBins)
for (subj in idvector) {
  dd <- subset(spatgradError, id==subj)
  h <- hist(dd$selectedDist, breaks=histbounds, plot=F)
  densities[subj,] <- as.numeric(h$density)
}
Dens <- Confint(Bakeman(densities))
binwidths <- diff(histbounds)
bincenters <- histbounds[1:nBins] + 0.5*binwidths
library("Hmisc")
x11(10,8)
layout(matrix(1:2,1,2))
errbar(1:5, y=tgrad0[1,], yplus=tgrad0[2,], yminus=tgrad0[3,], xlab="Transposition Distance", ylab="P(Response)/P(Opportunity)",
       xlim=c(0.5,5.5), ylim=c(0,0.25), pch=21, bg="black", type="b", cex=1.3)
par(new=T)
errbar(1:5, y=tgrad1[1,], yplus=tgrad1[2,], yminus=tgrad1[3,], xlab="Transposition Distance", ylab="P(Response)/P(Opportunity)",
       xlim=c(0.5,5.5), ylim=c(0,0.25), pch=22, bg="white", type="b", cex=1.3)
legend(5,0.25,c("No interference","Interference"),lty=1,pch=20+c(1:2),pt.bg=c("black", "white"),pt.cex=1.3, xjust=1)

errbar(bincenters, y=Dens[1,], yplus=Dens[2,], yminus=Dens[3,], 
       xlab="Euclidean Distance", ylab="P(Selection|Error)", 
       xlim=c(0.5,max(histbounds)+0.5), ylim=c(0,0.3), add=F, type="b", cex=1.3)
xcoord <- cbind(histbounds,histbounds)
ycoord <- cbind(rep(0,nBins+1), rep(0.05,nBins+1))
for (lin in 1:dim(xcoord)[1]) lines(x=xcoord[lin,], y=ycoord[lin,], col="red")

