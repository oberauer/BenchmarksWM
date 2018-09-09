# Read and pre-process data of Experiment 4 of Thalmann et al. (2018)
# More complete analysis scripts can be found on OSF

# clear workspace
rm(list = ls());

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

### subject nr. 9 has begun twice --> do not forget to delete first part of that data in second attempt before further processing
data <- read.table("Chunking.Experiment4.dat", header = F)
names(data) <- c("subject", "trial", "chunk.pres", "t.pres", "listtype", "stim.id", "t.rec", "t.ch.pres", "s_1", "s_2",
                 "s_3", "rt_1", "rt_2", "rt_3", "f_1", "f_2", "f_3", "t_1", "t_2", "t_3")
timepoint_chunk <- data$t.ch.pres
# save(timepoint_chunk, file = "timepoint.chunk.Rda")

# there should be 90 chunks --> good
nchunks <- nrow(subset(data, chunk.pres == 1 & listtype == 1))

# each id should be counted three times --> good
chunks <- subset(data, listtype == 1)
nl <- subset(data, listtype == 2)
data <- rbind(chunks, nl)
table(data$stim.id)

# each chunk/nl should be presented equally often in 1st, 2nd or 3rd position --> good
table(chunks$t.pres)
table(nl$t.pres)

# bring data in long format
data.long <- reshape(data, varying = 9:20, sep = "_", direction = 'long')
# introduce overall input serial position from 1-9
data.long$inp.sp.all <- (data.long$t.pres-1)*3+data.long$time
data.long$out.sp.all <- (data.long$t.rec-1)*3+data.long$time
# define factors as factors
data.long$chunk.pres <- as.factor(data.long$chunk.pres)
levels(data.long$chunk.pres) <- c("No Chunk Present", "Chunk Present")
data.long$listtype <- as.factor(data.long$listtype)
levels(data.long$listtype) <- c("Chunk", "New List")
data.long$t.rec <- as.factor(data.long$t.rec)
data.long <- data.long[order(data.long$trial, data.long$t.pres),]
data.long$t.pres <- as.factor(data.long$t.pres)
data.long$time <- as.factor(data.long$time)
data.long$t.ch.pres <- as.factor(data.long$t.ch.pres)

# save df for later lenient scoring analysis
data.free <- data.long


# create var that states when singleton/chunk was recalled in a trial
data.long.small.out <- data.frame()
for (s in unique(data.long$subject)){
  tmp <- data.long[data.long$subject == s,]
  for (t in unique(tmp$trial)){
    trial <- tmp[tmp$trial == t,]
    
    trial$cond <- 0
    trial$cond <- trial$t.pres[trial$listtype=="Chunk"][1]
    
    # variable for chunk in list position 1
    trial$lp1.bef <- 0 
    comp <- trial$t.rec[trial$t.pres==1][1]
    trial$lp1.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))
    
    # variable for chunk/singleton in list position 2
    trial$lp2.bef <- 0 
    comp <- trial$t.rec[trial$t.pres==2][1]
    trial$lp2.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))
    
    # variable for chunk in list position 3
    trial$lp3.bef <- 0 
    comp <- trial$t.rec[trial$t.pres==3][1]
    trial$lp3.bef <- as.numeric(as.numeric(trial$t.rec)-as.numeric(comp))
    
    # add variable that states whether chunk was recalled before
    trial$rec.ch.bef <- 0
    if(any(trial$listtype=="Chunk")){
      trial$rec.ch.bef[trial$listtype!="Chunk"] <- as.numeric(as.numeric(trial$t.rec[trial$listtype!="Chunk"]) > 
                                                                 as.numeric(trial$t.rec[trial$listtype=="Chunk"][1]))
    }
    data.long.small.out <- rbind(data.long.small.out, trial)
  }
}
data.long.small.out$cond <- as.numeric(data.long.small.out$cond)
data.long.small.out$cond[is.na(data.long.small.out$cond)]<-4
tmp1 <- as.numeric(data.long.small.out$lp1.bef > 0 & data.long.small.out$cond == 1)
tmp2 <- as.numeric(data.long.small.out$lp2.bef > 0 & data.long.small.out$cond == 2)
tmp3 <- as.numeric(data.long.small.out$lp3.bef > 0 & data.long.small.out$cond == 3)

data.long.small.out$rec.small.bef <- as.factor(tmp1 + tmp2 + tmp3)
levels(data.long.small.out$rec.small.bef) <- c("New List", "Chunk")

data.long.small.out$n.prev.rec <- data.long.small.out$out.sp.all-1

# take new lists out
nl.long <- subset(data.long.small.out, listtype == "New List")
nl.long$t.ch.pres <- as.numeric(nl.long$t.ch.pres)
nl.long$t.pres <- as.numeric(nl.long$t.pres)
# code whether a chunk was presented before or after
nl.long$ch.bef <- 0
nl.long$ch.bef <- as.numeric(nl.long$t.ch.pres < nl.long$t.pres)
nl.long$ch.aft <- 0
nl.long$ch.aft[nl.long$t.ch.pres != 4] <- as.numeric(nl.long$t.ch.pres[nl.long$t.ch.pres != 4] > nl.long$t.pres[nl.long$t.ch.pres != 4])
nl.long$ch.aft[nl.long$t.ch.pres == 4] <- 0
nl.long$ch.aft <- as.factor(nl.long$ch.aft)
levels(nl.long$ch.aft) <- c("No", "Yes")
nl.long$ch.bef <- as.factor(nl.long$ch.bef)
levels(nl.long$ch.bef) <- c("No", "Yes")
nl.long$t.ch.pres <- as.factor(nl.long$t.ch.pres)
nl.long$t.pres <- as.factor(nl.long$t.pres)
# new variable coding whether chunk at all, before or after (with three levels)
nl.long$when <- "No Chunk"
nl.long$when[nl.long$chunk.pres == "Chunk Present" & nl.long$ch.bef == "Yes"] <- "Chunk Before"
nl.long$when[nl.long$chunk.pres == "Chunk Present" & nl.long$ch.aft == "Yes"] <- "Chunk After"
nl.long$when <- as.factor(nl.long$when)
nl.long$when <- factor(nl.long$when, levels = c("No Chunk", "Chunk After", "Chunk Before"))
#introduce variables coding when chunk before or after was presented
nl.long$ch.p.bef <- 0
nl.long$ch.p.bef[nl.long$ch.bef == "Yes"] <- nl.long$t.ch.pres[nl.long$ch.bef == "Yes"]
nl.long$ch.p.aft <- 0
nl.long$ch.p.aft[nl.long$ch.aft == "Yes"] <- nl.long$t.ch.pres[nl.long$ch.aft == "Yes"]

nl.long$t.pres <- as.numeric(as.character(nl.long$t.pres))
nl.long$t.ch.pres <- as.numeric(as.character(nl.long$t.ch.pres))

nl.long$ch.p.bef[nl.long$t.ch.pres != 4 & nl.long$ch.p.bef != 0] <- nl.long$t.ch.pres[nl.long$t.ch.pres != 4 & nl.long$ch.p.bef != 0] -
  nl.long$t.pres[nl.long$t.ch.pres != 4 & nl.long$ch.p.bef != 0]
nl.long$ch.p.aft[nl.long$t.ch.pres != 4 & nl.long$ch.p.aft != 0] <- nl.long$t.ch.pres[nl.long$t.ch.pres != 4 & nl.long$ch.p.aft != 0] -
  nl.long$t.pres[nl.long$t.ch.pres != 4 & nl.long$ch.p.aft != 0]

nl.long$ch.p.bef <- as.factor(nl.long$ch.p.bef)
nl.long$ch.p.aft <- as.factor(nl.long$ch.p.aft)
nl.long$t.pres <- as.factor(nl.long$t.pres)
nl.long$t.ch.pres <- as.factor(nl.long$t.ch.pres)

nl.aft.long <- nl.long[nl.long$when != "Chunk Before",]
nl.bef.long <- nl.long[nl.long$when != "Chunk After",]

