# Read and pre-process data of Experiment 1 of Thalmann et al. (2018)
# More complete analysis scripts can be found on OSF

# clear workspace
rm(list = ls());

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

memdat<-read.table("Chunking.Experiment1.dat", 
                   col.names=c("subject", "session", "trial", "chunk1", "chunk2", 
                               "setsize1", "setsize2", "time_pres",
                               "time_rec", "seracc_p1", "seracc_p2", "seracc_p3", 
                               "seracc_p4", "RT1", "RT2", "RT3", "RT4",
                               "freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"))
# meaning of columns:
# subject: id
# session: two experimental sessions (each à 1 h)
# trial: trial nr. note. there were two lists (upper and lower) per trial
# chunk1: type of the firstly presented list
# chunk2: type of the secondly presented list
# setsize1: size of the firstly presented list
# setsize2: size of the secondly presented list
# time_pres: presented as the first (== 1, upper) or the second (== 2, lower) list?
# time_rec: recalled first(== 1) or second (== 2)
# seracc_p1 - seracc_p4: serial recall accuracy for the respective list in forward order
# RT1 - RT4: reaction time for recalling the respective list in forward order
# freeacc_p1 - freeacc_p4: recall accuracy (lenient scoring) in forward order
# n.b. -999/999 are fillers for two-item lists

# NAs where no items were presented
not_pres <- memdat$seracc_p3 == 999
memdat$seracc_p3[not_pres] <- NA
memdat$seracc_p4[not_pres] <- NA
memdat$RT3[not_pres] <- NA
memdat$RT4[not_pres] <- NA
memdat$freeacc_p3[not_pres] <- NA
memdat$freeacc_p4[not_pres] <- NA

# recode time of recall for time_pres == 2 (wrongly coded in matlab code)
# first subset first and second rows of a trial
try1st <- seq(1,nrow(memdat), by = 2)
try2nd <- seq(2,nrow(memdat), by = 2)

first <- memdat[try1st,]
second <- memdat[try2nd,]
filter1 <- first$time_pres == 2 & first$time_rec ==1
filter2 <- first$time_pres == 2 & first$time_rec ==2
first$time_rec[filter1==1] <- 2
first$time_rec[filter2==1] <- 1
second$time_rec[filter1==1] <- 1
second$time_rec[filter2==1] <- 2

memdat_new <- rbind(first, second)
memdat <- memdat_new

# factors as factors
memdat$chunk1<-as.factor(memdat$chunk1)
memdat$chunk2<-as.factor(memdat$chunk2)
memdat$setsize1<-as.factor(memdat$setsize1)
memdat$setsize2<-as.factor(memdat$setsize2)
memdat$time_pres<-as.factor(memdat$time_pres)
memdat$time_rec<-as.factor(memdat$time_rec)

# integrate serial recall acc, free recall acc and rt in one df
memdat_sermem <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec","freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4","RT1", "RT2", "RT3", "RT4"), measured = c("seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4"))
memdat_sermem$seracc <- memdat_sermem$value
memdat_freemem <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec", "seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4", "RT1", "RT2", "RT3", "RT4"), measured = c("freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"))
memdat_freemem$freeacc <- memdat_freemem$value
memdat_rt <- melt(memdat, id = c("subject", "session", "trial", "chunk1", "chunk2", "setsize1", "setsize2", "time_pres", "time_rec", "seracc_p1", "seracc_p2", "seracc_p3", "seracc_p4", "freeacc_p1", "freeacc_p2", "freeacc_p3", "freeacc_p4"), measured = c("RT1", "RT2", "RT3", "RT4"))
memdat_rt$rt <- memdat_rt$value

memdat<-cbind(memdat_sermem[,1:9], memdat_sermem[,18], memdat_sermem[,20], memdat_rt[,20], memdat_freemem[,20])
memdat<-rename(memdat, c("memdat_sermem[,18]"="sp", "memdat_sermem[,20]"="seracc", "memdat_freemem[,20]"="freeacc", "memdat_rt[,20]"="rt"))

names(memdat)[13] <- "freeacc"
names(memdat)[12] <- "rt"
names(memdat)[11] <- "seracc"
names(memdat)[10] <- "sp"
names(memdat)[4] <- "chunk_this"
names(memdat)[5] <- "chunk_other"
names(memdat)[6] <- "setsize_this"
names(memdat)[7] <- "setsize_other"

levels(memdat$sp) <- c("seracc_p1"="1", "seracc_p2"="2", "seracc_p3"="3", "seracc_p4"="4")
levels(memdat$time_rec) <- c("1"="Recall First", "2"="Recall Last")
levels(memdat$time_pres) <- c("1"="Presentation First", "2"="Presentation Last")
levels(memdat$chunk_this) <- c("0"="New List", "1"="Chunk")
levels(memdat$chunk_other) <- c("0"="New List", "1"="Chunk")

memdat <- memdat[!is.na(memdat$seracc),]

