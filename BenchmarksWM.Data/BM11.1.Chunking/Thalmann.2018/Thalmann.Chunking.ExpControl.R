# Read and pre-process the "control experiment" from Thalmann et al. (2018)
# More complete analysis scripts can be found on OSF

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

# nb: t.pres refers to point of sublist presentation (first, second, or third sublist)

# clear workspace
rm(list = ls());

data <- read.table("Chunking.Controlexperiment.dat", header = F)
names(data) <- c("subject", "trial", "t.pres", "list.type", "list.id", "t.rec", "cond", "s_1", "s_2",
                 "s_3", "rt_1", "rt_2", "rt_3", "f_1", "f_2", "f_3", "t_1", "t_2", "t_3")
####### checking and preprocessing ###########
# do some checking
sum(data$list.type == 10)
sum(data$list.type == 3)
sum(data$list.type == 1)
sum(data$cond == 1 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 2 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 3 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 4 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
sum(data$cond == 5 & data$list.type == 3 & data$t.pres == 2 & data$t.rec == 1)
# --> looks good

# chunk and New List 3 ids should be counted twice per participant --> good
table(data$list.id)

chunks <- data[data$list.type == 10,]
nl1 <- data[data$list.type == 1,]
nl3 <- data[data$list.type == 3,]
table(chunks$t.pres, chunks$t.rec)
table(nl1$t.pres, nl1$t.rec)
# 3*30 in sublist position 1, 5*30 in sublist position 2, 3*30 in sublist position 3
table(nl3$t.pres, nl3$t.rec)

# mark at which sublist position ch3 OR! nl1 was presented in the trial
data$t.small.load <- 0
data$t.small.load[data$cond == 1] <- 1
data$t.small.load[data$cond == 2] <- 3
data$t.small.load[data$cond == 4] <- 1
data$t.small.load[data$cond == 5] <- 3

# was a chunk presented before or after a not-chunked list?
data$small.load.bef <- 0
data$small.load.bef[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] < data$t.pres[data$t.small.load != 0]
data$small.load.aft <- 0
data$small.load.aft <- as.numeric(data$t.small.load > data$t.pres)

# what is the position of the small load compared to a new list?
data$relpos.small.load <- 0
data$relpos.small.load[data$t.small.load != 0] <- data$t.small.load[data$t.small.load != 0] - data$t.pres[data$t.small.load != 0]

# introduce variable, whether whole sublist was recalled correctly
data$s_12 <- data$s_1
data$s_22 <- data$s_2
data$s_32 <- data$s_3
data$s_12[data$list.type == 1 & data$cond == 1] <- 1
data$s_22[data$list.type == 1 & data$cond == 1] <- 1
data$s_22[data$list.type == 1 & data$cond == 2] <- 1
data$s_32[data$list.type == 1 & data$cond == 2] <- 1

data$wholesl_1 <- data$s_12*data$s_22*data$s_32
data$wholesl_2 <- data$s_12*data$s_22*data$s_32
data$wholesl_3 <- data$s_12*data$s_22*data$s_32

# bring data in long format
data.long <- reshape(data, varying = c(8:19, 27:29), sep = "_", direction = 'long')
# Chunk 1s were presented at time == 3 in cond 1 and at time == 1 in condition 3
incl1 <- as.logical((as.numeric(data.long$list.type == 1 & data.long$cond == 1 & data.long$time < 3)-1)*-1)
data.long <- data.long[incl1,]
incl2 <- as.logical((as.numeric(data.long$list.type == 1 & data.long$cond == 2 & data.long$time > 1)-1)*-1)
data.long <- data.long[incl2,]

# transposition 999 = 0
data.long$t[data.long$t == 999] <- 0
# introduce overall input serial position from 1-7/9
# therefore, first determine sublist length
data.long <- data.long[order(data.long$t.pres),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$inp.sp.all <- seq(1,7,1)
cond345$inp.sp.all <- seq(1,9,1)
# now overall output serial posiiton 
data.long <- rbind(cond12, cond345)
data.long <- data.long[order(data.long$t.rec),]
data.long <- data.long[order(data.long$trial),]
data.long <- data.long[order(data.long$subject),]
cond12 <- data.long[data.long$cond < 3,]
cond345 <- data.long[data.long$cond > 2,]
cond12$out.sp.all <- seq(1,7,1)
cond345$out.sp.all <- seq(1,9,1)
data.long <- rbind(cond12, cond345)

# denote factors
vars = which(names(data.long) %in% c("subject", "cond", "t.pres", "list.type", "t.rec", "small.load.bef", "small.load.aft", "inp.sp.all", "out.sp.all"))
data.long[,vars] <- lapply(data.long[,vars], as.factor)

# name variables where necessary
levels(data.long$list.type) <- c("Singleton", "New List", "Chunk")
levels(data.long$cond) <- c("Singleton First", "Singleton Last", "Baseline", "Chunk First", "Chunk Last")

# create var that states when singleton/chunk was recalled in a trial
data.long.small.out <- data.frame()
for (s in unique(data.long$subject)){
  tmp <- data.long[data.long$subject == s,]
  for (t in unique(tmp$trial)){
    trial <- tmp[tmp$trial == t,]
    
    # variable for chunk/singleton in list position 1
    trial$lp1.bef <- 0 
    comp <- trial$t.rec[trial$t.pres==1][1]
    trial$lp1.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))
    
    # variable for chunk/singleton in list position 3
    trial$lp3.bef <- 0 
    comp <- trial$t.rec[trial$t.pres==3][1]
    trial$lp3.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))
    
    data.long.small.out <- rbind(data.long.small.out, trial)
  }
}
tmp1 <- as.numeric(data.long.small.out$lp1.bef > 0 & data.long.small.out$cond == "Chunk L1")
tmp2 <- as.numeric(data.long.small.out$lp1.bef > 0 & data.long.small.out$cond == "Singleton L1")*2
tmp3 <- as.numeric(data.long.small.out$lp3.bef > 0 & data.long.small.out$cond == "Chunk L3")
tmp4 <- as.numeric(data.long.small.out$lp3.bef > 0 & data.long.small.out$cond == "Singleton L3")*2

data.long.small.out$rec.small.bef <- as.factor(tmp1 + tmp2 + tmp3 + tmp4)
levels(data.long.small.out$rec.small.bef) <- c("New List", "Chunk", "Singleton")
