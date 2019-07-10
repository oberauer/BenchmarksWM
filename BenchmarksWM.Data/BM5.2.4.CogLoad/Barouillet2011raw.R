# Reads the unaggregated raw data of the Updating, Nback, and Stroop experiments in Barrouillet et al. (2011)

rm(list=ls())

library("readxl")
library(psych)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

MUdat <- as.data.frame(matrix(NA, 24*36, 19))
names(MUdat) <- c("id", "condition", "listlength", 
                  "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", 
                  "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8")

Muspan <- as.data.frame(matrix(NA, 24, 3))
names(Muspan) <- c("id", "sspan", "muspan")

tIdx <- 1
for (id in 1:24) {
  sheetname <- paste0("pp", id)
  dat <- read_excel("MUvsSS_IndRecall.xls",sheet=sheetname,range="A3:K77", col_names=F)  
  muspan <- 0
  sspan <- 0
  for (row in 1:75) {
    if (!is.na(dat[row,1])) {
      setsize <- as.numeric(strsplit(as.character(dat[row,1]), " ")[[1]][1])
      MUdat[tIdx,1] <- id
      MUdat[tIdx,2] <- dat[row, 11]
      MUdat[tIdx,3] <- setsize
      MUdat[tIdx,4:(4+setsize-1)] <- dat[row-1,2:(2+setsize-1)]
      MUdat[tIdx,12:(12+setsize-1)] <- dat[row,2:(2+setsize-1)]
      correct <- mean(dat[row-1,2:(2+setsize-1)] == dat[row,2:(2+setsize-1)])
      if (!is.na(correct)) {
        if (correct == 1) {
          if (dat[row, 11] == "MU") muspan <- muspan + 1/3
          if (dat[row, 11] == "SS") sspan <- sspan + 1/3
        }
      }
      tIdx <- tIdx + 1
    }
  }
  Muspan[id, "id"] <- id
  Muspan[id, "sspan"] <- 2 + sspan + 2   # add to 2 because starts with setsize 3, and another 2 for memory load of simple span
  Muspan[id, "muspan"] <- 2 + muspan + 2  # add 2 extra because of memory load of MU
}

# From Sophie Portrat's email (7.3.2019): Note : For MUvsSS task, the RTs  from pp. 7/13/14/15/18 are not available.
Muspan <- subset(Muspan, !is.element(id, c(7,13,14,15,18)))


NBdat <- as.data.frame(matrix(NA, 24*30, 17))
names(NBdat) <- c("id", "condition", "listlength", 
                  "L1", "L2", "L3", "L4", "L5", "L6", "L7", 
                  "R1", "R2", "R3", "R4", "R5", "R6", "R7")

Nbspan <- as.data.frame(matrix(NA, 24, 3))
names(Nbspan) <- c("id", "nbspan0", "nbspan2")

tIdx <- 1
IDs <- c(1,3,5:13,15:23,26:29)
for (id in 1:24) {
  dat <- read_excel("NBack_IndRecall.xls",sheet=id+1,range="B4:K64", col_names=F)  
  nbspan0 <- 0
  nbspan2 <- 0
  for (row in 1:61) {
    if (!is.na(dat[row,10])) {
      NBdat[tIdx,1] <- IDs[id]
      NBdat[tIdx,2] <- dat[row, 10]
      list <- dat[row-1,1:7]
      setsize <- sum(!is.na(list))
      NBdat[tIdx,3] <- setsize
      NBdat[tIdx,4:10] <- list
      NBdat[tIdx,11:17] <- dat[row,1:7]
      correct <- mean(list[1:setsize] == dat[row,1:setsize])
      if (!is.na(correct)) {
        if (correct == 1) {
          if (dat[row, 10] == "2B") nbspan2 <- nbspan2 + 1/3
          if (dat[row, 10] == "0B") nbspan0 <- nbspan0 + 1/3
        }
      }
      tIdx <- tIdx + 1
    }
  }
  Nbspan[id, "id"] <- id
  Nbspan[id, "nbspan0"] <- 2 + nbspan0 + 2   # add to 2 because starts with setsize 3, and another 2 for memory load of simple span
  Nbspan[id, "nbspan2"] <- 2 + nbspan2 + 2  # add 2 extra because of memory load of MU
}


StroopColdat <- as.data.frame(matrix(NA, 20*24, 15))
names(StroopColdat) <- c("id", "condition", "listlength", 
                  "L1", "L2", "L3", "L4", "L5", "L6",  
                  "R1", "R2", "R3", "R4", "R5", "R6")

Stroopspan <- as.data.frame(matrix(NA, 20, 3))
names(Stroopspan) <- c("id", "neutral", "incongruent")

tIdx <- 1
for (id in 1:20) {
  dat <- read_excel("StroopColor_IndRecall.xls",sheet=id+1, range="A2:I52", col_names=F)  
  neutral <- 0
  incongruent <- 0
  for (row in 1:51) {
    if (!is.na(dat[row,1])) {
      setsize <- as.numeric(strsplit(as.character(dat[row,1]), " ")[[1]][1])
      StroopColdat[tIdx,1] <- id
      StroopColdat[tIdx,2] <- dat[row, 9]
      StroopColdat[tIdx,3] <- setsize
      StroopColdat[tIdx,4:(4+setsize-1)] <- dat[row-1,2:(2+setsize-1)]
      StroopColdat[tIdx,10:(10+setsize-1)] <- dat[row,2:(2+setsize-1)]
      correct <- mean(dat[row-1,2:(2+setsize-1)] == dat[row,2:(2+setsize-1)])
      if (!is.na(correct)) {
        if (correct == 1) {
          if (dat[row, 9] == "M") neutral <- neutral + 1/3
          if (dat[row, 9] == "S") incongruent <- incongruent + 1/3
        }
      }
      tIdx <- tIdx + 1
    }
  }
  Stroopspan[id, "id"] <- id
  Stroopspan[id, "neutral"] <- 2 + neutral   # add to 2 because starts with setsize 3
  Stroopspan[id, "incongruent"] <- 2 + incongruent
}


StroopDigdat <- as.data.frame(matrix(NA, 20*24, 15))
names(StroopDigdat) <- c("id", "condition", "listlength", 
                         "L1", "L2", "L3", "L4", "L5", "L6",  
                         "R1", "R2", "R3", "R4", "R5", "R6")

StroopDigspan <- as.data.frame(matrix(NA, 20, 3))
names(StroopDigspan) <- c("id", "neutral", "incongruent")

tIdx <- 1
for (id in 1:20) {
  dat <- read_excel("StroopDigit_IndRecall.xls",sheet=id+1, range="A2:I52", col_names=F)  
  neutral <- 0
  incongruent <- 0
  for (row in 1:51) {
    if (!is.na(dat[row,1])) {
      setsize <- as.numeric(strsplit(as.character(dat[row,1]), " ")[[1]][1])
      StroopDigdat[tIdx,1] <- id
      StroopDigdat[tIdx,2] <- dat[row, 9]
      StroopDigdat[tIdx,3] <- setsize
      StroopDigdat[tIdx,4:(4+setsize-1)] <- dat[row-1,2:(2+setsize-1)]
      StroopDigdat[tIdx,10:(10+setsize-1)] <- dat[row,2:(2+setsize-1)]
      correct <- mean(dat[row-1,2:(2+setsize-1)] == dat[row,2:(2+setsize-1)])
      if (!is.na(correct)) {
        if (correct == 1) {
          if (dat[row, 9] == "L") neutral <- neutral + 1/3
          if (dat[row, 9] == "S") incongruent <- incongruent + 1/3
        }
      }
      tIdx <- tIdx + 1
    }
  }
  StroopDigspan[id, "id"] <- id
  StroopDigspan[id, "neutral"] <- 2 + neutral   # add to 2 because starts with setsize 3
  StroopDigspan[id, "incongruent"] <- 2 + incongruent
}



#### Read Times 

MUrtdat <- NULL
for (id in c(1:6, 8:12)) {   # pp7 is empty
  dat <- read_excel("MuvsSS_IndRTs_pp1to12.xls", sheet=id, col_names=T)
  dat$id <- id
  MUrtdat <- rbind(MUrtdat, dat[,c("id", "Trial", "condition", "Time")])
}
for (id in c(13,15:24)) {   # pp14 has no valid data
  dat <- read_excel("MuvsSS_IndRTs_pp13to24.xls", sheet=id-12, col_names=T)
  dat$id <- id
  MUrtdat <- rbind(MUrtdat, dat[,c("id", "Trial", "condition", "Time")])
}
MUrtdat$condition[MUrtdat$condition=="ps"] <- "MU0"  # these names make ordering of conditions more transparent in the integrated data (below)
MUrtdat$condition[MUrtdat$condition=="mu"] <- "MU2"

# exclude 3 more participants who have very few data points (Barrouillet et al. 2011, Appdx, say they excluded 5 participants with too few data)
# From Sophie Portrat's email (7.3.2019): Note : For MUvsSS task, the RTs  from pp. 7/13/14/15/18 are not available.
MUrtdat$included <- 1
MUrtdat$included[MUrtdat$id==13 | MUrtdat$id==15 | MUrtdat$id==18] <- 0
MUrtdat <- subset(MUrtdat, included==1)
MUrtdat$meanRT <- as.numeric(MUrtdat$Time)
MUrtdat$ptime <- MUrtdat$meanRT*8
MU <- aggregate(cbind(ptime, meanRT) ~ id+condition, data=MUrtdat, FUN=mean)
MU$CL <- 8*MU$meanRT/(8*1500+2000)  # available time: 1.5 s per updating step, plus 2 s initialization of the distractor phase

## Add the span values computed above
MU$span <- 0
MU$span[MU$condition=="MU0"] = Muspan[,"sspan"]
MU$span[MU$condition=="MU2"] = Muspan[,"muspan"]


NbackRTdat <- as.data.frame(read_excel("NBack_IndRTs.xls", col_names=T))
NbackRTdat <- NbackRTdat[, c("pp", "Trial", "training", "conditie", "Time")]
names(NbackRTdat) <- c("id", "Trial", "practice", "condition", "Time")
NbackRTdat <- subset(NbackRTdat, practice==0)
NbackRTdat$meanRT <- as.numeric(NbackRTdat$Time)
NbackRTdat$ptime <- NbackRTdat$meanRT*8
Nback <- aggregate(cbind(ptime, meanRT) ~ id+condition, data=NbackRTdat, FUN=mean)
Nback$CL <- 8*Nback$meanRT/(8*1500+500)  # available time per letter was 1500, plus 500 ms before onset of the first letter

# Eliminate the 5 subjects with insufficient MU accuracy
Nback <- Nback[is.element(Nback$id, IDs), ]

## Add the span values computed above
Nback$span <- 0
Nback$span[Nback$condition=="0b"] = Nbspan[,"nbspan0"]
Nback$span[Nback$condition=="2b"] = Nbspan[,"nbspan2"]


## For the Stroop tasks, times were not recorded because responses were oral - RTs from pilot study were used instead

ColorStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="B13:F33"))
names(ColorStroop) <- c("id", "ptimeNeutral", "ptimeColor", "spanNeutral", "spanColor")
CStroopNeutral <- ColorStroop[,c("id", "ptimeNeutral", "spanNeutral")]
names(CStroopNeutral) <- c("id", "ptime", "aggrSpan")
CStroopNeutral$condition <- "ColorStroopNeutral"
CStroopColor <- ColorStroop[,c("id", "ptimeColor", "spanColor")]
names(CStroopColor) <- c("id", "ptime", "aggrSpan")
CStroopColor$condition <- "ColorStroopColor"
ColorStroop <- rbind(CStroopNeutral, CStroopColor)
ColorStroop$meanRT <- ColorStroop$ptime/8   # divide by number of trials
ColorStroop$totaltime <- 8500
ColorStroop$CL <- ColorStroop$ptime/ColorStroop$totaltime  

## Add the span values computed above
ColorStroop$span <- 0
ColorStroop$span[ColorStroop$condition=="ColorStroopNeutral"] = Stroopspan[,"neutral"]
ColorStroop$span[ColorStroop$condition=="ColorStroopColor"] = Stroopspan[,"incongruent"]


NumberStroop <- as.data.frame(read_excel("Barrouillet.2011.xlsx",sheet="Inhibition",range="I13:M33"))
names(NumberStroop) <- c("id", "ptimeNeutral", "ptimeNumber", "spanNeutral", "spanNumber")
NStroopNeutral <- NumberStroop[,c("id", "ptimeNeutral", "spanNeutral")]
names(NStroopNeutral) <- c("id", "ptime", "aggrSpan")
NStroopNeutral$condition <- "NumberStroopNeutral"
NStroopNumber <- NumberStroop[,c("id", "ptimeNumber", "spanNumber")]
names(NStroopNumber) <- c("id", "ptime", "aggrSpan")
NStroopNumber$condition <- "NumberStroopNumber"
NumberStroop <- rbind(NStroopNeutral, NStroopNumber)
NumberStroop$meanRT <- NumberStroop$ptime/8  # divide by the number of trials to obtain mean RT
NumberStroop$totaltime <- 8500
NumberStroop$CL <- NumberStroop$ptime/NumberStroop$totaltime  

## Add the span values computed above
NumberStroop$span <- 0
NumberStroop$span[NumberStroop$condition=="NumberStroopNeutral"] = StroopDigspan[,"neutral"]
NumberStroop$span[NumberStroop$condition=="NumberStroopNumber"] = StroopDigspan[,"incongruent"]

### The Parity and Size Judgment data are not available in an unaggregated format, so read aggregated data:

# Parity judgment experiment
Parity4 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="B17:D33")
names(Parity4) <- c("id", "ptime", "span")
Parity4$condition <- "Parity-4"
Parity4$meanRT <- Parity4$ptime/4  # divide by number of trials

Parity6 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="E17:F33")
Parity6 <- cbind(Parity4[,"id"], Parity6)
names(Parity6) <- c("id", "ptime", "span")
Parity6$condition <- "Parity-6"
Parity6$meanRT <- Parity6$ptime/6  # divide by number of trials

Parity8 <- read_excel("Barrouillet.2011.xlsx",sheet="Retrieval",range="G17:H33")
Parity8 <- cbind(Parity4[,"id"], Parity8)
names(Parity8) <- c("id", "ptime", "span")
Parity8$condition <- "Parity-8"
Parity8$meanRT <- Parity8$ptime/8  # divide by number of trials

Parity <- rbind(Parity4, Parity6, Parity8)
Parity$totaltime <- 6900
Parity$CL <- Parity$ptime/Parity$totaltime  # cognitive load = processing time / total time

# Size judgment experiment
Size4 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="B17:D33")
names(Size4) <- c("id", "ptime", "span")
Size4$condition <- "Size-4"
Size4$meanRT <- Size4$ptime/4  # divide by number of trials

Size6 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="E17:F33")
Size6 <- cbind(Size4[,"id"], Size6)
names(Size6) <- c("id", "ptime", "span")
Size6$condition <- "Size-6"
Size6$meanRT <- Size6$ptime/6  # divide by number of trials

Size8 <- read_excel("Barrouillet.2011.xlsx",sheet="Response Selection",range="G17:H33")
Size8 <- cbind(Size4[,"id"], Size8)
names(Size8) <- c("id", "ptime", "span")
Size8$condition <- "Size-8"
Size8$meanRT <- Size8$ptime/8  # divide by number of trials

Size <- rbind(Size4, Size6, Size8)
Size$totaltime <- 6900
Size$CL <- Size$ptime/Size$totaltime  


#### Put all the data together and plot

selVar <- c("id", "condition", "meanRT", "ptime", "CL", "span")
Data <- as.data.frame(rbind(Parity[,selVar], Size[,selVar], ColorStroop[,selVar], NumberStroop[,selVar], MU[,selVar], Nback[,selVar]))
Data$condition <- as.factor(Data$condition)

CL <- aggregate(CL ~ condition, data=Data, FUN=mean)
Span <- aggregate(span ~ condition, data=Data, FUN=mean)

pt=c(24, 24, 23, 23, 24, 24, 23, 23, 21, 21, 21, 22, 22, 22) # markers corresponding to Barrouillet et al (2011)
col=rep("black", 14)
ptcol=c("red", "red", "blue", "blue", "red", "red", "blue", "blue", "black", "black", "black",  "green", "green", "green")
Conditions <- unique(Data$condition)
xlim <- c(0,1)
ylim <- c(0,9)


x11()
plot(CL$CL, Span$span, xlim=xlim, ylim=ylim, type="p", pch=pt, col=col, bg=ptcol, 
     xlab="CL", ylab="Span")        
errorCircles(x="CL", y="span", data=Data, group="condition", add=T,
             xlim = xlim, ylim = ylim, labels="",
             arrow.len = 0.05, alpha = 0.05, sd = FALSE, bars = TRUE, circles = F, 
             pch=pt, xlab="", ylab="") 
abline(a=8.13, b=-8.33)  # regression line reported in Barrouillet et al. (2011)
legend(1,9,c("Updating", "Inhibition", "Response Selection", "Retrieval"), 
       pch=c(24,23,22,21), pt.bg=c("red", "blue", "green", "black"), yjust=1, xjust=1)


# Put together the mean processing times and CLs in all conditions

meanTimes <- aggregate(cbind(meanRT, ptime, CL) ~ condition, data=Data, FUN=mean)



### Comment K. Oberauer (June 2019)
# The plot does not match the published plot in some regards 
# One source of the discrepancy is the CL computed for the updating tasks. For the Memory-Updating condition, the CL computed here is .53, based on mean RT = 930 ms. In the Appendix of Barrouillet et al. (2011) that mean is reported as 994 ms, which would imply an even larger CL of .57. The Cl of the updating condition in the published plot is ~.44, which is the value calculated from the aggregated data. 
# Another source of the discrepancy is that the mean RT for the 0-back/2-back tasks - which roughly match the ones reported in the Appendix of Barrouillet et al (2011) -- imply higher CL values (0.33 and 0.38, respectively) than the ones used for the plot (.24 and .28, respectively, which are the values computed from the aggregate data). 
