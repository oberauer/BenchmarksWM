####### Read Data from Gilchrist & Cowan (2014) Experiment #######

rm(list=ls())
graphics.off()
library("readxl")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

# Load data for simple and complex span
exp1 = read_excel("./Gilchrist.Cowan/Gilchrist & Cowan 2014_Experiment 1 data.xlsx", sheet="Raw Data_Experiment1")  
exp2 = read_excel("./Gilchrist.Cowan/Gilchrist & Cowan 2014_Experiment 2 data.xlsx", sheet="Raw Data_Experiment 2")  

