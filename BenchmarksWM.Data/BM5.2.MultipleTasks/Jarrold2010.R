####### Read Jarrold et al (2010) data #######

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of this script as the current directory

rm(list=ls())
graphics.off()

library("readxl")

# Load data, pre-process

CondNames <- c("ZeroSix", "OneFive", "TwoFour", "ThreeThree", "FourTwo", "FiveOne", "ZeroSix")
CondCols <- c(3, 8, 13, 19, 24, 29, 34)

Data <- as.data.frame(matrix(NA, 50*7*10*6, 6))
names(Data) <- c("id", "trial", "condition", "serpos", "item", "response")

excluded <- c(1, 19, 27, 29, 33, 35, 37, 43)  # these participants were excluded by Jarrold et al.
Data <- Data[Data$id != excluded, ]

rowNum <- 1
for (subj in 1:50) {
  d = as.data.frame(read_excel("Jarrold2010.detailed.xls", sheet=subj, range="A1:AL69"))  
  for (cond in 1:7) {
    for (trial in 1:10) {
      trialname = paste0("T", trial)
      subdat <- subset(d, Trial==trialname)
      for (serpos in 1:6) {
        condition <- names(d)[CondCols[cond]]
        Data[rowNum, 1] <- subj
        Data[rowNum, 2] <- trial
        Data[rowNum, 3] <- condition
        Data[rowNum, 4] <- serpos
        Data[rowNum, 5] <- subdat[serpos, CondCols[cond]]
        resp <- subdat[serpos, CondCols[cond]+1]
        response <- resp  # default
        if (resp == "c") response <- Data[rowNum, 5]  # correct responses are coded as "c", so set response to the list item
        if (resp == "x") response <- "Omission"
        if (is.na(resp)) response <- "Omission" 
        Data[rowNum, 6] <- response
        rowNum <- rowNum + 1
      }
    }
  }
}

write.table(Data, file="Jarrold2010.long.txt", row.names=F)



