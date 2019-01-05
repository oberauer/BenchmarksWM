# Auditory-Deviant and Changing-State Effect: Preregistered Replication by Raoul Bell and colleagues (2018)

library(Hmisc)
library("tidyr")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory
setwd('..')
source(paste(dirname(getwd()), "/functions/lineplot.ci3.R", sep=""))
source(paste(dirname(getwd()), "/functions/BakemanL.R", sep=""))
setwd(paste(dirname(getwd()), '/BM6.IrrelevantSound/Raoul.Bell.Replication', sep=""))

data <- read.table("BENCHMARK_RAW_DATA.txt", header=F)
names(data) <- c("id", "session", "trial", "condition", 
                 "stim1", "stim2", "stim3", "stim4", "stim5", "stim6", "stim7", "stim8",
                 "REM",
                 "resp1", "resp2", "resp3", "resp4", "resp5", "resp6", "resp7", "resp8",
                 "SCORE", "Numremembered", "rt", 
                 "distr1", "distr2", "distr3", "distr4", "distr5", "distr6", "distr7", "distr8", "distr9", "distr10")
# conditions: SS = steady-state; CS = changing-state; DEV = auditory deviant

data$condnum <- 1
data$condnum[data$condition=="CS"] <- 2
data$condnum[data$condition=="DEV"] <- 3
data$corr1 <- as.numeric(data$stim1 == data$resp1)
data$corr2 <- as.numeric(data$stim2 == data$resp2)
data$corr3 <- as.numeric(data$stim3 == data$resp3)
data$corr4 <- as.numeric(data$stim4 == data$resp4)
data$corr5 <- as.numeric(data$stim5 == data$resp5)
data$corr6 <- as.numeric(data$stim6 == data$resp6)
data$corr7 <- as.numeric(data$stim7 == data$resp7)
data$corr8 <- as.numeric(data$stim8 == data$resp8)

# turn into long format
datalong <- data %>% gather("serpos", "correct", corr1, corr2, corr3, corr4, corr5, corr6, corr7, corr8)
datalong <- datalong[, c("id", "session", "trial", "condnum", "serpos", "correct")]

datalong$serpos <- sapply( strsplit(datalong$serpos , split="rr"), function(x) (as.numeric(x[2])) )

# Aggregate, apply Bakeman-McArthur correction for computing within-subjects confidence intervals
dagg <- aggregate(correct ~ id+serpos+condnum, data=datalong, FUN=mean)
dagg <- BakemanL(dagg, id="id", dv="correct")


x11(10,7)
lineplot.ci3(dagg, dv="correct", iv=c("serpos", "condnum"),  xlim=c(0.5,8.8), ylim=c(0,1), 
             xlab="Serial Position", ylab="P(correct)")
legend(0.5, 0, c("Steady", "Changing", "Deviant"), pch=15:17, yjust=0)


