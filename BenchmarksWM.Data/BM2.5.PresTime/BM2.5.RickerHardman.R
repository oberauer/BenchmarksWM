####### Read Data from Ricker & Hardman (2017), Experiment 2 #######

rm(list=ls())
graphics.off()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
source(paste(dirname(getwd()), "/functions/lineplot.ci.R", sep=""))

ptypes <- c(21:25, 21:25)
colors <- c("black", "grey", "white", "grey80", "grey20", "black", "white")

# Load data 
data = read.table("RickerHardman2017Exp2.dat", header=T)

# Column labels are explained below. Stimulus presentation, response, and error are listed in degrees of 
# circular angle. For the x and y columns, zero refers to the top of the circle, negative angles are toward 
# the left side of the circle, and positive angles are toward the right side of the circle.
# 
# sub = Participant number
# block = Experiment block number 
# trial = Trial number within the current block
# ctime = Consolidation time that followed each item mask, during which the screen is blank
# x1 = Presented angle for the first memory item
# y1 = Participant response angle for the first memory item
# err1 = Response error for the first memory item
# rt1 = Reaction time for the first memory item
# x2 = Presented angle for the second memory item
# .
# rt4 = Reaction time for the fourth memory item

