#-----------------------------------------------------
#Read Data from:
#Lange, Cerella, & Verhaeghen JEPLMC 37, 2011, 608-620
#Exp. 1, 2, 4
#-----------------------------------------------------

#remove all data and functions
rm(list=ls())

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)

# Read Data ---------------------------------------------------------------------------------------------------
#for Exp.1
E1_data <- read.csv("Exp1_data.csv", header=T, sep=",") 
E2_data <- read.csv("Exp2_data.csv", header=T, sep=",")
E4_data <- read.csv("Exp4_data.csv", header=T, sep=",") 

###############################
# Variable Coding Exp. 1, 2 & 4 ---------------------------------------------------------------------------------------------

# General information on terminology:
# Temporal input order: sequential presentation of memory digits from 1 to n items
# Spatial input position: Frames are numbered in reading direction (left to right, up to down) from 1 to n see Figure 1 in Lange et al., 2011; input: position of the memory digits
# Temporal output order: Sequential presentation of the probe items from 1 to n
# Spatial output position: Frame position of the probe from 1 to n

 
# General information on differences between Expts:
# NOTE: Coding of temporal order and spatial position of memory digits (study items) and probe digits (recognition items) are different in Exp. 2 from Exp. 1 & 4. The related variables are outnr, mem_dig, prb_dig, out_pos, in_pos; 



# id 		  subject id
# Group	  blocked design latin square, 12 subgroups of participants with different serial orders of condition (1..4) and set size (3..5); 
# sex	    sex (1=female; 2=male)
# age	    age
# Session	Number of session (1..2)
# Memo	  1 = memory task, 0 = magnitude task
# example	1 = example task, 0 = no example task (experimental trials); to avoid confusion, example trials are already deleted in this data set
# Con	    conditions: 1 = forward, 2 = fixed irregular, 3 = backward, 4 = random
# Nblock	Number of block, with 12 blocks per session (1..12); 
# N	      set size (3..5)
# Trial 	counter for the trial number (n=3: 1..12, n=4: 1..15, n=5: 1..20)
# outnr	  Exp.1&4: temporal output order of the probed item; outnr=1 is the first tested item, outnr=2 is the second tested item, and so on  
# Trialnr	    maximum number of trials for this n (12, 15, 20)
# Out_allnr	  continuous counter of output responses (1..60 for experimental trials per block per session)
# Matchkey	  codes key assignment (self-selected): 1 = left key denotes "yes"; 2 = right key denotes "yes"
# Pressedkey	codes pressed key: 1 = left, 2 = right 
# Corr	      1=correct answer, 0 = wrong answer
# Mem_dig	    to-be-remembered digit, digits are listed here with temporal presentation order across rows (same as in outnr);  
#             (not coded for magnitude judgment task)
# Prb_dig	    digit that is probed; there is no direct relation between the prb_dig and mem_dig in the same row (besides in the forward condition).
# Out_pos	    Exp. 1&4: Temporal and spatial input position of the item tested in the current row. Example: out_pos=2 means, that the RT in this row is related to the second learned item (temporal input order =2), that was learned at the second frame position (spatial input position=2), and was probed at the second frame position (spatial output position=2) (Exp. 2 has two other coding variables instead)
# In_pos	    Exp.2: spatial input position of the memory digit, e.g., in the backward condition, for the first row belonging to this trial, mem_dig was presented first but at the last frame position. This is coded as in_pos=5 for n=5 and outnr=1 (not coded in Exp. 1&4).
# In_nr	      Exp.2: temporal input order of the probed item, e.g., in_nr=4 the RT in this row relates to the item learned fourth. (not coded in Exp. 1&4; not coded for memo=0)
# Pre_RT	    Time to initiate the trial
# RT	        RT for probe recognition/magnitude judgement task (raw data)
# Pos	        classifies the positive probes (the probe in the same row): 1 = positive probe, 0 = negative probe/ for magnitude task: 1 = larger 5, 0 = smaller five
# Neg	        classifies the negative probes (the probe in the same row): 0 = positive probe, 1 = intrusion negative probe (probe was in the memory list of this trial at a different position), 2 = extra-list negative probe (probe was not in the memory list of this trial); not coded for magnitude judgment task
#             Percentages of all items (50% are not coded because memo = 0):
#             Exp.1: positive=25%, intrusion=8.3%, extra-list=16.7% 
#             Exp.2: positive=24.5%, intrusion=8.5%, extra-list=17%; slight deviations from Exp. 1 in the forward condition, 0.7% of the probes were selected randomly by mistake in the procedure; 
#             Exp.4: positive=25%, intrusion=12.5%, extra-list=12.5 %; deviation due to different balancing schema 
# RT_r	      RTs recoded: Set wrong answers to missing; Set outliers to missing
#                                                                                   
#  ---------------------
# Outlier analysis
# RT_mean	   Means based on individual RTs for memo-con-pos; 
# RT_sd	     SDs based on individual RTs for memo-con-pos; 
# Cut	       cut=RT_mean + 3*RT_sd; RTs > cut or RTs < 100 are set to missing in RT_r
#                                                                                   
# Exp       Experiment Number 1,2,4                                                                                  