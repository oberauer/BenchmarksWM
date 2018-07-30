####### Read Data from Towse, Cowan, Hitch, & Horton, Experimental Psychology (2008), Experiment 2 #######

rm(list=ls())
graphics.off()

library("readxl")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

# Load data for simple and complex span
timedatLL2 = as.data.frame(read_excel("./Towse.ComplexSpan/Experimental Psychology E2 recall timing data.xlsx", sheet="LL2", range="A1:O85"))  
timedatLL3 = as.data.frame(read_excel("./Towse.ComplexSpan/Experimental Psychology E2 recall timing data.xlsx", sheet="LL3", range="A1:O85")) 
recalldat = as.data.frame(read_excel("./Towse.ComplexSpan/Experimental Psychology computer records.xlsx", sheet="Collated", range="A3:Q87"))

replaceDash <- function(x) {
  if (!is.na(x)) {
    if (x=="-") {x=NA} else x = x 
  } else {x = NA}
}

#timedatLL3[] <- lapply(timedatLL3, FUN=replaceDash)  # does not work

for (row in 1:dim(timedatLL3)[1]) {
  for (col in 1:dim(timedatLL3)[2]) {
    timedatLL2[row,col] <- replaceDash(timedatLL2[row,col])   
    timedatLL3[row,col] <- replaceDash(timedatLL3[row,col])   
  }
}
for (row in 1:dim(recalldat)[1]) {
  for (col in 1:dim(recalldat)[2]) {
    recalldat[row,col] <- replaceDash(recalldat[row,col])   
  }
}

