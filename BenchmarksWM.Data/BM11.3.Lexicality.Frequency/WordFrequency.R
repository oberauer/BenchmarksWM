# Read data of Quinlan, Roodenrys, & Miller (2017), Exp. 4

# clear workspace
rm(list = ls());
library("readxl")
source(paste(dirname(getwd()), "/functions/plot.confint.R", sep=""))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

ntrials <- 60
nsubj <- 91

Demographics <- as.data.frame(matrix(NA, nsubj, 3))
names(Demographics) <- c("id", "Sex", "Age")
Materials <- as.data.frame(matrix(NA, nsubj*ntrials, 9))
names(Materials) <- c("id", "trial", "Freq", "P1", "P2", "P3", "P4", "P5", "P6")
Order <- Materials
Responses <- Materials
Accuracy <- as.data.frame(matrix(NA, nsubj*ntrials, 10))
names(Accuracy) <- c("id", "trial", "Task", "Cond", "P1", "P2", "P3", "P4", "P5", "P6")

for (id in 1:nsubj) {
  demo <- read_excel("Quinlan.2017.Exp4.xlsx", sheet=id+2, range="A2:A3")
  materials <- read_excel("Quinlan.2017.Exp4.xlsx", sheet=id+2, range="A4:G64")
  order <- read_excel("Quinlan.2017.Exp4.xlsx", sheet=id+2, range="A66:G126")
  responses <- read_excel("Quinlan.2017.Exp4.xlsx", sheet=id+2, range="A128:G188")
  accuracy <- read_excel("Quinlan.2017.Exp4.xlsx", sheet=id+2, range="H128:O188")
  Demographics[id, ] <- cbind(id, demo)
  rows <- ((id-1)*ntrials + 1):(id*ntrials)
  Materials[rows, 1] <- id
  Materials[rows, 2] <- 1:ntrials
  Materials[rows, 3:9] <- materials
  Responses[rows, 1] <- id
  Responses[rows, 2] <- 1:ntrials
  Responses[rows, 3:9] <- responses
  Accuracy[rows, 1] <- id
  Accuracy[rows, 2] <- 1:ntrials
  Accuracy[rows, 3:10] <- accuracy
}

# Order: for serial reconstruction, order in which the words were arranged for reconstruction (for serial recall, this appears to code accuracy: 1 = correct, 0 = incorrect word in each list position)
# Response: for serial reconstruction, position in the array that was selected (if it matches "order", the response is correct)
# Accuracy: Task: 1 = reconstruction, 2 = recall, Condition: 1- High F reconstruction, 2 - Low F reconstruction, 3 - High F recall, 4, Low F recall. The remaining 6 columns are the accuracy in each serial position 

aggdat <- aggregate(cbind(P1,P2,P3,P4,P5,P6) ~ id+Cond, data=Accuracy, FUN=mean)
pointcolor <- c("black", "black", "white", "white")
x11()
for (cond in 1:4) {
  plot.confint(data=aggdat[aggdat$Cond==cond,], varvector=3:8, xvector=1:6, 
               ylim=c(0,1), xlab="Serial Position", ylab="P(correct)", pch=21+(cond%%2), bg=pointcolor[cond])
  par(new=T)
}
legend(0.5, 0, c("ROO HF", "ROO LF", "Recall HF", "Recall LF"), pch=c(22,21,22,21), pt.bg=pointcolor, yjust=0)


