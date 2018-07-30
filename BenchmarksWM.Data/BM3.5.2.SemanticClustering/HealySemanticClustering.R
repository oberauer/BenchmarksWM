#Read Data of Healy et al (2014)

#remove all data and functions
rm(list=ls())

# Set the paths ---------------------------------------------------------------------------------------------
# to automatically get the path of the current folder use rstudioapi
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
myDir <- dirname(current_path)
setwd(myDir)

# Read Data ---------------------------------------------------------------------------------------------------

data <- read.table("Healy14.dat", header=F)  
names(data) <- c("subject", "session", 
                 "word1", "word2", "word3", "word4", "word5", "word6", "word7", "word8", 
                 "word9", "word10", "word11", "word12", "word13", "word14", "word15", "word16", 
                 "recword1", "recword2", "recword3", "recword4", "recword5", "recword6", "recword7", 
                 "recword8", "recword9", "recword10", "recword11", "recword12", "recword13", "recword14", 
                 "recword15", "recword16", "recword17", "recword18", "recword19", "recword20", "recword21", 
                 "recword22", "recword23", "recword24", "recword25", "recword26", "recword27", "recword28", 
                 "recinpos1", "recinpos2", "recinpos3", "recinpos4", "recinpos5", "recinpos6", "recinpos7", 
                 "recinpos8", "recinpos9", "recinpos10", "recinpos11", "recinpos12", "recinpos13", "recinpos14", 
                 "recinpos15", "recinpos16", "recinpos17", "recinpos18", "recinpos19", "recinpos20", "recinpos21", 
                 "recinpos22", "recinpos23", "recinpos24", "recinpos25", "recinpos26", "recinpos27", "recinpos28", 
                 "rectime1", "rectime2", "rectime3", "rectime4", "rectime5", "rectime6", "rectime7", 
                 "rectime8", "rectime9", "rectime10", "rectime11", "rectime12", "rectime13", "rectime14", 
                 "rectime15", "rectime16", "rectime17", "rectime18", "rectime19", "rectime20", "rectime21", 
                 "rectime22", "rectime23", "rectime24", "rectime25", "rectime26", "rectime27", "rectime28") 

# wordX: identification number of word presented in list position X
# recwordY: identification number of word recalled in output position Y
# recinposY: list position of word recalled in output position Y
# rectimeY: cumulative response time of word recalled in output position Y
