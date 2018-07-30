####### Read Data from Gilchrist & Cowan (2014) Experiment #######

rm(list=ls())
graphics.off()
library("readxl")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

# Load data 
data = read_excel("Cowan2002JML.xls", sheet="Raw data with RTs", range="A1:AD3841")  

# Modality = visual (V) or auditory (A)
# StimGroup = grouped (G) or ungrouped (U)
# Part/Whole = partial recall (P) or whole recall (W)
# StartPos = start position of recall: 1, 4, or 7
# TBRs = to-be-remembered stimuli
# Response = reported stimuli (... for list items not to be reported in partial recall)
# i1 to i9: accuracy of recall in list positions 1:9
# rt1 to rt9: cumulative response times for recalling items in list positions 1:9; 
# these times start with the StartPos (so for instance, when StartPos=7, rt7 is the shortest time because that is the list position the person started at)

